{ haskell
, haskellPackages
, curl
, stdenv
, callPackage
, fetchgitPrivate
, git
, writeText
, postgresql
, writeScript
, openssh
, docker
, lib
, helpers
, writeScriptBin
}:

let
  hcl = haskellPackages.hcl;
  holborn-api = haskellPackages.holborn-api;
  holborn-repo = haskellPackages.holborn-repo;
  holborn-ssh = haskellPackages.holborn-ssh;
  dockerImageBuilder = (import ../holborn-ssh/docker.nix {}).sshImage;

  repoPort = 8080;
  rawRepoPort = 8081;
  apiPort = 8002;
  sshPort = 3333;

  pgPort = 5444;
  pgUser = "test-user";

  # TODO: Attempt to hard code these for now. Will need to update to calculate
  # from Docker itself.
  dockerHost = "172.17.0.1";
  dockerClient = "172.17.0.2";

  sshClientUser = "git";
  sshAuthorizedKeysUser = "holborn";

  holborn-ssh-docker = dockerImageBuilder {
    port = sshPort;
    clientUser = sshClientUser;
    serverUser = sshAuthorizedKeysUser;
    openssh = openssh;
    holborn-ssh = holborn-ssh;
    api-url = "http://${dockerHost}:${toString apiPort}";
  };

  makeSSHKey =
    { name, type, comment }:
    let dir = stdenv.mkDerivation {
      inherit name;
      buildInputs = [ openssh ];
      phases = "installPhase";
      preferLocalBuild = true;
      installPhase = ''
        mkdir $out
        ssh-keygen -q -t ${type} -N "" -C "${comment}" -f "$out/${name}"
      '';
    };
    public = "${dir}/${name}.pub";
    fullKey = lib.removeSuffix "\n" (builtins.readFile public);
    in
    {
      inherit comment fullKey public;
      type = "ssh-${type}";
      private = "${dir}/${name}";
      key = lib.removeSuffix " ${comment}" (lib.removePrefix "ssh-${type} " fullKey);
    };

  # ssh tries to create an ~/.ssh directory if it's not given a config file,
  # and it uses the home directory found in getpwent (see
  # https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=706194)
  #
  # On NixOS, this is /var/empty, which is not writeable. Thus, make our own
  # config file.
  holborn-ssh-client-config = writeText "client-config" ''
    UserKnownHostsFile=/dev/null
    StrictHostKeyChecking=no
    IdentityFile = ${testKey.private}
  '';

  initial_sql = ../holborn-api/sql/initial.sql;

  comment = "client-key";
  testKey = makeSSHKey {
    inherit comment;
    name = "holborn-openssh-test-key";
    type = "rsa";
  };

  # Insert the pubkey into the database for testing
  insertTestKeySql =
    writeText "insertTestKey.sql" ''
    insert into "user" (username, email) values
         ( 'alice'
         , 'alice@example.com'
         );
    insert into public_key (submitted_pubkey, owner_id, verified, readonly) values
         ( '${testKey.fullKey}'
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
  insert into "org_repo" (id, name, description, org_id, hosted_on) values
       ( ${toString repoId}
       , '${repoName}'
       , 'test repository owned by ${ownerName}'
       , 1
       , '${dockerHost}:${toString repoPort}'
       );
  '';

in
writeScriptBin "holborn-ssh-authz-test" ''
      set -e
      echo "*** holborn-ssh-authz-test"

      ${docker}/bin/docker load < ${holborn-ssh-docker}

      pg_database=$(mktemp -d)
      working_dir=$(mktemp -d)
      trap 'kill $(jobs -p); rm -rf $working_dir' EXIT # kill everything before exit

      ${postgresql}/bin/initdb -D $pg_database
      ${postgresql}/bin/postgres -D $pg_database -p ${toString pgPort} &
      sleep 2

      ${postgresql}/bin/createuser -p ${toString pgPort} ${pgUser}
      ${postgresql}/bin/createdb -p ${toString pgPort} -O ${pgUser} $pg_database

      ${postgresql}/bin/psql -p ${toString pgPort} -U ${pgUser} -d $pg_database -qf ${initial_sql}
      ${postgresql}/bin/psql -p ${toString pgPort} -U ${pgUser} -d $pg_database -qf ${insertTestKeySql}
      ${postgresql}/bin/psql -p ${toString pgPort} -U ${pgUser} -d $pg_database -qf ${insertRepositorySql}

      ${holborn-api}/bin/holborn-api-server \
        --port=${toString apiPort} \
        --postgres-database=$pg_database \
        --postgres-user=${pgUser} \
        --postgres-port=${toString pgPort} \
        --repo-hostname=${dockerHost} \
        --repo-http-port=${toString repoPort} \
        --repo-git-port=${toString rawRepoPort} &

      ${holborn-repo}/bin/holborn-repo \
        --http-port=${toString repoPort} \
        --git-port=${toString rawRepoPort} \
        --repo-root=${test-repos} &

      ${docker}/bin/docker run holborn-ssh:latest &

      # Wait for server to become ready
      ${hcl}/bin/hcl-wait-for-port --host ${dockerClient} --port ${toString sshPort} --timeout 10
      ${hcl}/bin/hcl-wait-for-port --host ${dockerHost} --port ${toString repoPort} --timeout 10
      ${hcl}/bin/hcl-wait-for-port --host ${dockerHost} --port ${toString rawRepoPort} --timeout 10
      ${hcl}/bin/hcl-wait-for-port --host ${dockerHost} --port ${toString apiPort} --timeout 10

      # Before we try to do git stuff, make sure we can fetch the key from the API server
      ${holborn-ssh}/bin/holborn-authorized-keys --key ${testKey.key} -t ${testKey.type} --api-url "http://${dockerHost}:${toString apiPort}"

      # Clone the test repository
      pushd $working_dir
      GIT_SSH_COMMAND="${openssh}/bin/ssh -F ${holborn-ssh-client-config}" ${git}/bin/git clone --verbose ssh://${sshClientUser}@${dockerClient}:${toString sshPort}/org/hello
      observed=$(${git}/bin/git --git-dir ${repoName}/.git rev-parse HEAD)
      popd

      # The same content?
      expected=$(${git}/bin/git --git-dir ${helpers.test-repo}/.git rev-parse HEAD)
      [[ $expected == $observed ]]
      echo "SUCCESS"
''

