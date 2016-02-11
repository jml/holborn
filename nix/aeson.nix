{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, containers, deepseq, dlist, ghc-prim, hashable, HUnit, mtl
, QuickCheck, scientific, stdenv, syb, template-haskell
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, time, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson";
  version = "0.10.0.0";
  sha256 = "19kp33rfivr4d3myyr8xn803wd7p8x5nc4wb3qvlgjwgyqjaxvrz";
  buildDepends = [
    attoparsec base blaze-builder bytestring containers deepseq dlist
    ghc-prim hashable mtl scientific syb template-haskell text time
    transformers unordered-containers vector
  ];
  testDepends = [
    attoparsec base bytestring containers ghc-prim HUnit QuickCheck
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text time unordered-containers vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
