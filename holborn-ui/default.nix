{ nodejs, stdenv, haskellPackages }:
stdenv.mkDerivation {
  name = "holborn-ui";
  src = ./.;
  propagatedBuildInputs = [
    nodejs
    haskellPackages.purescript
    haskellPackages.psc-ide
  ];
  # npm install pulp bower
}
