{ mkDerivation, alex, array, base, blaze-builder, bytestring, Cabal
, containers, happy, hspec, mtl, QuickCheck, stdenv, text
, utf8-light, utf8-string
}:
mkDerivation {
  pname = "language-javascript";
  version = "0.6.0.4";
  sha256 = "1yz5n01njgfkq352rk01dym42v9jx4x8r7am9yzsbrnvizsqc3l5";
  libraryHaskellDepends = [
    array base blaze-builder bytestring containers mtl text utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [
    array base blaze-builder bytestring Cabal containers hspec mtl
    QuickCheck utf8-light utf8-string
  ];
  homepage = "http://github.com/erikd/language-javascript";
  description = "Parser for JavaScript";
  license = stdenv.lib.licenses.bsd3;
}
