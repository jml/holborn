{ haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText }:

let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
  holborn-api = haskellPackages.callPackage ../holborn-api {};
  test-repos = callPackage ./test-repo.nix {};
  holborn-openssh-source = fetchgitPrivate {
    url = "git@bitbucket.org:tehunger/holborn-ssh.git";
    sha256 = "91e998af03249db570d00262aa5b7b39720b2899b1aa3e86e76bfd10d0299a37";
    rev = "HEAD";
  };
  holborn-ssh = callPackage "${holborn-openssh-source}/nix" {};

  holborn-ssh-testconfig = writeText "testconfig" ''
    UsePrivilegeSeparation=no
    HostKey=${holborn-ssh}/etc/ssh/ssh_host_rsa_key
    HostKey=${holborn-ssh}/etc/ssh/ssh_host_dsa_key
    Port=3333
    PidFile=/dev/null
    HolbornApiEndpoint=http://127.0.0.1:8082
  '';

in
stdenv.mkDerivation {
  name = "integration-tests";
  buildDepends = [ git holborn-ssh ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      # Make this script more readable by placing git into PATH
      export PATH=$PATH:${git}/bin:${holborn-ssh}/bin

      # GIT_SSH_COMMAND requires at least git 2.3
      export GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

      # Run ssh + repo server
      ${holborn-ssh}/bin/sshd -D -e -f ${holborn-ssh-testconfig} &
      PORT=8082 ${holborn-api}/bin/holborn-api-server &
      echo "REPOROOT ${test-repos}"
      REPO_ROOT=${test-repos} ${holborn-repo}/bin/holborn &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # TODO wait for servers to respond instead of sleep 1
      sleep 2s

      # Clone the test repository
      mkdir $out
      pushd $out
      git clone --verbose ssh://127.0.0.1:3333/org/hello >> $out/integration-test-log
      popd

      # The same content?
      diff ${test-repos}/org/hello/hello $out/hello/hello
  '';
}
