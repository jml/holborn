{ nodejs, stdenv, haskellPackages, purescript-derive-lenses, sassc }:
stdenv.mkDerivation {
  name = "holborn-ui";
  src = ./.;
  propagatedBuildInputs = [
    nodejs
    haskellPackages.purescript
    haskellPackages.psc-ide
    sassc
    # purescript-derive-lenses  # not working due to build failure.
  ];
}
