{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText }:
let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
  hcl = haskell.lib.dontHaddock (haskellPackages.callPackage ../hcl {});

  test-repos = callPackage ./test-repo.nix {};

  repoPort = "8080";
  rawPort = "8081";
in
stdenv.mkDerivation {
  name = "integration-protocol-test-1";
  buildDepends = [ holborn-repo git ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      set -ex
      echo "*** holborn-protocol-test-1"

      # Make this script more readable by placing git into PATH
      export PATH=$PATH:${git}/bin

      # Run server
      PORT=${repoPort} RAW_PORT=${rawPort} REPO_ROOT=${test-repos} ${holborn-repo}/bin/holborn-repo &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # Wait for HTTP server to become ready
      ${hcl}/bin/hcl-wait-for-port ${repoPort} --timeout 5
      ${hcl}/bin/hcl-wait-for-port ${rawPort} --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      GIT_TRACE=2 git clone --verbose http://127.0.0.1:${repoPort}/v1/repos/100 >> $out/integration-test-log
      popd

      # The same content?
      ls $out
      diff ${test-repos}/100/hello $out/100/hello
  '';
}
