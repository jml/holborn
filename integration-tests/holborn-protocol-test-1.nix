# Run like:
# nix-build  integration-tests.nix

# Use Nix 15.09
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;

let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};

  test-repo = stdenv.mkDerivation {
  name = "test-repo";
  srcs = ./holborn-protocol-test-1.nix;
  phases = "installPhase";
  installPhase = ''
      export PATH=$PATH:${git}/bin
      mkdir $out
      cd $out
      git init
      echo "hello" > hello
      git add hello
      git commit -m"hello"
  '';
  };

in
stdenv.mkDerivation {
  name = "integration-tests";
  buildDepends = [ holborn-repo git systemd ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      # Make this script more readable by placing git into PATH
      export PATH=$PATH:${git}/bin

      # 2) Run server
      REPO=${test-repo} ${holborn-repo}/bin/holborn &

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
      diff ${test-repo}/hello $out/hello/hello
  '';
}
