{ stdenv, haskellPackages, callPackage, pkgs }:

# TODO: Add frontend to this.

let
  hcl = haskellPackages.callPackage ../hcl {};
  holborn-api = haskellPackages.callPackage ../holborn-api {};
  holborn-repo = haskellPackages.callPackage ../holborn-repo {};
in
stdenv.mkDerivation {
  name = "build-everything";
  buildInputs = [ hcl holborn-api holborn-repo ];
  src = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      mkdir $out
  '';
}
