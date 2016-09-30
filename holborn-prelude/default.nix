{ mkDerivation, base, hashable, mtl, protolude, safe, stdenv, tasty
, tasty-hspec, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "holborn-prelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hashable mtl protolude safe text ];
  testHaskellDepends = [
    base hashable mtl protolude safe tasty tasty-hspec tasty-hunit
    tasty-quickcheck text
  ];
  description = "Standard prelude for Holborn";
  license = stdenv.lib.licenses.unfree;
}
