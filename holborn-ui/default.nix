{ nodejs, stdenv, haskellPackages, purescript-derive-lenses }:
stdenv.mkDerivation {
  name = "holborn-ui";
  src = ./.;
  propagatedBuildInputs = [
    nodejs
    haskellPackages.purescript
    haskellPackages.psc-ide
    # purescript-derive-lenses  # not working due to build failure.
  ];
}
