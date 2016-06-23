{ mkDerivation, aeson, attoparsec, base, blaze-html, blaze-markup
, bytestring, either, errors, exceptions, holborn-common-types
, holborn-prelude, hspec-wai, hspec-wai-json, http-api-data
, http-client, http-reverse-proxy, http-types, jwt
, optparse-applicative, postgresql-simple, process, QuickCheck
, servant-docs, servant-server, stdenv, tasty, tasty-hspec
, tasty-hunit, tasty-quickcheck, text, time, transformers, wai
, wai-cors, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base blaze-html blaze-markup bytestring either
    errors exceptions holborn-common-types holborn-prelude
    http-api-data http-client http-reverse-proxy http-types jwt
    postgresql-simple servant-docs servant-server text time
    transformers wai warp
  ];
  executableHaskellDepends = [
    base holborn-common-types holborn-prelude optparse-applicative
    servant-server wai wai-cors warp
  ];
  testHaskellDepends = [
    aeson base bytestring errors holborn-common-types holborn-prelude
    hspec-wai hspec-wai-json http-client http-types process QuickCheck
    servant-server tasty tasty-hspec tasty-hunit tasty-quickcheck
    transformers wai
  ];
  license = stdenv.lib.licenses.unfree;
}
