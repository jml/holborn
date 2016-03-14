{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText }:

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

in
stdenv.mkDerivation {
  name = "holborn-openssh-test";
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
      echo "REPO_ROOT ${test-repos}"
      REPO_ROOT=${test-repos} ${holborn-repo}/bin/holborn-repo &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # Wait for server to become ready
      ${hcl}/bin/hcl-wait-for-port 3333 --timeout 5
      ${hcl}/bin/hcl-wait-for-port 8080 --timeout 5
      ${hcl}/bin/hcl-wait-for-port 8081 --timeout 5
      ${hcl}/bin/hcl-wait-for-port 8082 --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      git clone --verbose ssh://127.0.0.1:3333/org/hello >> $out/integration-test-log
      popd

      # The same content?
      diff ${test-repos}/org/hello/hello $out/hello/hello
  '';
}
