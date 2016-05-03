{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText, postgresql, writeScript, openssh, lib, procps }:

let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
  holborn-api = haskellPackages.callPackage ../holborn-api {};
  test-repos = callPackage ./test-repo.nix {};
  holborn-ssh = callPackage ../nix/holborn-ssh.nix {};
  hcl = haskell.lib.dontHaddock (haskellPackages.callPackage ../hcl {});

  holborn-ssh-testconfig = writeText "testconfig" ''
    UsePrivilegeSeparation=no
    HostKey=${holborn-ssh}/etc/ssh_host_rsa_key
    HostKey=${holborn-ssh}/etc/ssh_host_dsa_key
    Port=3333
    PidFile=/dev/null
    HolbornApiEndpoint=http://127.0.0.1:8082
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

  # Insert the pubkey into the database for testing
  insertTestKeySql =
    let pubkey = builtins.readFile "${testKey}/testkey.pub";
    in writeText "insertTestKey.sql" ''
    insert into public_key (name, submitted_pubkey, comparison_pubkey, owner_id, verified, readonly) values
         ( 'testkey'
         , '${pubkey}'
         , '${lib.removeSuffix " comment\n" pubkey}'
         , 1
         , true
         , false
         );
     select * from public_key;
  '';
in
stdenv.mkDerivation {
  name = "holborn-openssh-test";
  buildInputs = [ git holborn-ssh postgresql hcl procps holborn-api holborn-repo test-repos ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      set -ex
      echo "*** holborn-openssh-test"
      trap 'kill $(jobs -p)' EXIT # kill everything before exit

      # Make this script more readable by placing git into PATH
      export PATH=$PATH:${git}/bin:${holborn-ssh}/bin

      # GIT_SSH_COMMAND requires at least git 2.3
      export GIT_SSH_COMMAND="ssh -F ${holborn-ssh-client-config}"

      export HOLBORN_PG_PORT=5444
      export HOLBORN_PG_USER=test-user
      export HOLBORN_PG_DATABASE=test-db
      export PGPORT=$HOLBORN_PG_PORT
      initdb -D $HOLBORN_PG_DATABASE
      postgres -D $HOLBORN_PG_DATABASE -p $HOLBORN_PG_PORT &
      sleep 2

      createuser $HOLBORN_PG_USER
      createdb -O $HOLBORN_PG_USER $HOLBORN_PG_DATABASE
      export PGUSER=$HOLBORN_PG_USER
      export PGDATABASE=$HOLBORN_PG_DATABASE

      psql -f ${initial_sql}
      psql -f ${insertTestKeySql}

      # Run ssh + repo server
      ${holborn-ssh}/bin/sshd -D -e -f ${holborn-ssh-testconfig} &
      PORT=8082 ${holborn-api}/bin/holborn-api-server &
      echo "REPO_ROOT ${test-repos}"
      REPO_ROOT=${test-repos} ${holborn-repo}/bin/holborn-repo &

      # Wait for server to become ready
      hcl-wait-for-port 3333 --timeout 5
      hcl-wait-for-port 8080 --timeout 5
      hcl-wait-for-port 8081 --timeout 5
      hcl-wait-for-port 8082 --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      git clone --verbose ssh://127.0.0.1:3333/org/hello >> $out/integration-test-log
      popd

      # The same content?
      diff ${test-repos}/org/hello/hello $out/hello/hello
  '';
}
