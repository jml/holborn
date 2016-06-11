{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, bytestring-conversion, case-insensitive, directory, doctest
, filemanip, filepath, hspec, http-api-data, http-media, http-types
, mmorph, mtl, network-uri, QuickCheck, quickcheck-instances
, string-conversions, text, url, vault
, forked-servant
, stdenv
}:
mkDerivation rec {
  pname = "servant";
  version = "0.7.1";
  src = forked-servant;
  preCompileBuildDriver = ''
    cd ${pname}/
  '';
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring bytestring-conversion
    case-insensitive http-api-data http-media http-types mmorph mtl
    network-uri string-conversions text vault
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring directory doctest filemanip
    filepath hspec QuickCheck quickcheck-instances string-conversions
    text url
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
