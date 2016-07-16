{ mkDerivation, base, basic-prelude, mtl, safe, stdenv, tasty
, tasty-hspec, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "holborn-prelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base basic-prelude mtl safe text ];
  testHaskellDepends = [
    base basic-prelude mtl safe tasty tasty-hspec tasty-hunit
    tasty-quickcheck text
  ];
  description = "Standard prelude for Holborn";
  license = stdenv.lib.licenses.unfree;
}
