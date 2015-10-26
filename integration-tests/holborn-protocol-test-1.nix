{ haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText }:
let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};

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

      # 2) Run server
      REPOROOT=${test-repos} ${holborn-repo}/bin/holborn &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # TODO wait for server to respond instead of sleep 1
      sleep 1s

      # Clone the test repository
      mkdir $out
      pushd $out
      git clone --verbose http://127.0.0.1:8080/org/hello >> $out/integration-test-log
      popd

      # The same content?
      diff ${test-repos}/org/hello/hello $out/hello/hello
  '';
}
