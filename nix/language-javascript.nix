{ mkDerivation, alex, array, base, blaze-builder, bytestring, Cabal
, containers, fetchgit, happy, HUnit, mtl, QuickCheck, stdenv
, test-framework, test-framework-hunit, utf8-light, utf8-string
}:
mkDerivation {
  pname = "language-javascript";
  version = "0.5.14.6";
  src = fetchgit {
    url = "https://github.com/teh/language-javascript";
    sha256 = "8e267a07cc42413284d3c1a8eaf1ff0abbf252dee988efe9e2927038669313f6";
    rev = "22c8018f7e97aa791c4217489755a6c1052a655f";
  };
  libraryHaskellDepends = [
    array base blaze-builder bytestring containers mtl utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [
    array base blaze-builder bytestring Cabal containers HUnit mtl
    QuickCheck test-framework test-framework-hunit utf8-light
    utf8-string
  ];
  homepage = "http://github.com/erikd/language-javascript";
  description = "Parser for JavaScript";
  license = stdenv.lib.licenses.bsd3;
}
