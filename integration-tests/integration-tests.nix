# Run like:
# nix-build  integration-tests.nix

# Use unstable channel from Wed 19th of August:
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;

let
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
in
stdenv.mkDerivation {
  name = "integration-tests";
  buildDepends = [ holborn-repo git systemd ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
  export PATH=$PATH:${git}/bin
  ${holborn-repo}/bin/holborn &
  trap 'kill $(jobs -p)' EXIT
  sleep 1s

  mkdir $out
  cd $out
  git clone  http://127.0.0.1:8080/org/test > $out/hello
  '';
}
