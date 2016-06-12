{ mkDerivation, base, basic-prelude, stdenv, tasty, tasty-hspec
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "holborn-prelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base basic-prelude ];
  testHaskellDepends = [
    base tasty tasty-hspec tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
