{ stdenv, haskellPackages, callPackage, pkgs }:
stdenv.mkDerivation {
  name = "build-everything";
  buildInputs = with haskellPackages; [ hcl holborn-api holborn-repo holborn-proxy holborn-ssh ];
  src = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      mkdir $out
  '';
}
