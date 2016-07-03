{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText, postgresql, writeScript, openssh, lib
, helpers }:

let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
  holborn-api = haskellPackages.callPackage ../holborn-api {};
  holborn-ssh = callPackage ../nix/holborn-ssh.nix {};
  hcl = haskell.lib.dontHaddock (haskellPackages.callPackage ../hcl {});

  repoPort = "8080";
  rawRepoPort = "8081";
  apiPort = "8082";
  sshPort = "3333";

  pgPort = "5444";
  pgUser = "test-user";
  pgDatabase = "test-db";

  holborn-ssh-testconfig = writeText "testconfig" ''
    UsePrivilegeSeparation=no
    HostKey=${holborn-ssh}/etc/ssh_host_rsa_key
    HostKey=${holborn-ssh}/etc/ssh_host_dsa_key
    Port=${sshPort}
    PidFile=/dev/null
    HolbornApiEndpoint=http://127.0.0.1:${apiPort}
  '';

  # ssh tries to create an ~/.ssh directory if it's not given a config file,
  # and it uses the home directory found in getpwent (see
  # https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=706194)
  #
  # On NixOS, this is /var/empty, which is not writeable. Thus, make our own
  # config file.
  holborn-ssh-client-config = writeText "client-config" ''
    UserKnownHostsFile = /dev/null
    StrictHostKeyChecking = no
    IdentityFile = ${testKey}/testkey
  '';

  initial_sql = ../holborn-api/sql/initial.sql;

  testKey = stdenv.mkDerivation {
    name = "holborn-openssh-test-key";
    buildInputs = [ openssh ];
    phases = "installPhase";
    installPhase = ''
      mkdir $out
      ssh-keygen -t rsa -f $out/testkey -N "" -C "comment"
    '';
  };

  # Insert the public key into the database for testing
  insertTestKeySql =
    let pubkey = builtins.readFile "${testKey}/testkey.pub";
    in writeText "insertTestKey.sql" ''
    insert into "user" (username, email) values
         ( 'alice'
         , 'alice@example.com'
         );
    insert into public_key (name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly) values
         ( 'testkey'
         , '${pubkey}'
         , '${lib.removeSuffix " comment\n" pubkey}'
         , 1
         , true
         , false
         );
  '';

  repoId = 100;
  ownerName = "org";
  repoName = "hello";
  test-repos = helpers.repo-store "test-repos" [ { id = repoId; repo = helpers.test-repo; } ];

  insertRepositorySql = writeText "insertRepository.sql" ''
  -- Assume alice creates the repo, and that alice has id 1
  insert into "org" (orgname, created_by_id) values
       ( '${ownerName}'
       , 1
       );
  insert into "org_repo" (name, description, org_id, hosted_on) values
       ( '${repoName}'
       , 'test repository owned by ${ownerName}'
       , 1
       , '127.0.0.1:${repoPort}'
       );
  '';

in
stdenv.mkDerivation {
  name = "holborn-openssh-test";
  buildInputs = [ git holborn-ssh postgresql hcl holborn-api holborn-repo test-repos ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      set -e
      echo "*** holborn-openssh-test"
      trap 'kill $(jobs -p)' EXIT # kill everything before exit

      initdb -D ${pgDatabase}
      postgres -D ${pgDatabase} -p ${pgPort} &
      sleep 2

      createuser -p ${pgPort} ${pgUser}
      createdb -p ${pgPort} -O ${pgUser} ${pgDatabase}

      psql -p ${pgPort} -U ${pgUser} -d ${pgDatabase} -qf ${initial_sql}
      psql -p ${pgPort} -U ${pgUser} -d ${pgDatabase} -qf ${insertTestKeySql}
      psql -p ${pgPort} -U ${pgUser} -d ${pgDatabase} -qf ${insertRepositorySql}

      # Run ssh + repo server
      ${holborn-ssh}/bin/sshd -D -e -f ${holborn-ssh-testconfig} &

      ${holborn-api}/bin/holborn-api-server \
        --port=${apiPort} \
        --postgres-database=${pgDatabase} \
        --postgres-user=${pgUser} \
        --postgres-port=${pgPort} \
        --repo-hostname=127.0.0.1 \
        --repo-http-port=${repoPort} \
        --repo-git-port=${rawRepoPort} &

      # holborn-repo has an undocumented run-time dependency on git, and expects
      # it to be in the path.
      PATH=$PATH:${git}/bin ${holborn-repo}/bin/holborn-repo \
        --http-port=${repoPort} \
        --git-port=${rawRepoPort} \
        --repo-root=${test-repos} &

      # Wait for server to become ready
      hcl-wait-for-port --port ${sshPort} --timeout 5
      hcl-wait-for-port --port ${repoPort} --timeout 5
      hcl-wait-for-port --port ${rawRepoPort} --timeout 5
      hcl-wait-for-port --port ${apiPort} --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      # GIT_SSH_COMMAND requires at least git 2.3
      GIT_SSH_COMMAND="${holborn-ssh}/bin/ssh -F ${holborn-ssh-client-config}" GIT_TRACE=2 ${git}/bin/git clone --verbose ssh://127.0.0.1:${sshPort}/${ownerName}/${repoName}
      popd

      # The same content?
      expected=$(${git}/bin/git --git-dir ${helpers.test-repo}/.git rev-parse HEAD)
      observed=$(${git}/bin/git --git-dir $out/${repoName}/.git rev-parse HEAD)
      [[ $expected == $observed ]]
  '';
}
