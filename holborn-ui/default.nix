{ nodejs, stdenv, haskellPackages }:
stdenv.mkDerivation {
  name = "holborn-ui";
  src = ./.;
  propagatedBuildInputs = [
    nodejs
    haskellPackages.purescript
  ];
  # npm install pulp bower
}
