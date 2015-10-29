{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText }:
let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
  hcl = haskell.lib.dontHaddock (haskellPackages.callPackage ../hcl {});

  test-repos = callPackage ./test-repo.nix {};

in
stdenv.mkDerivation {
  name = "integration-tests";
  buildDepends = [ holborn-repo git ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      # Make this script more readable by placing git into PATH
      export PATH=$PATH:${git}/bin

      # Run server
      REPO_ROOT=${test-repos} ${holborn-repo}/bin/holborn-repo &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # Wait for HTTP server to become ready
      ${hcl}/bin/hcl-wait-for-port 8080 --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      git clone --verbose http://127.0.0.1:8080/org/hello >> $out/integration-test-log
      popd

      # The same content?
      diff ${test-repos}/org/hello/hello $out/hello/hello
  '';
}
