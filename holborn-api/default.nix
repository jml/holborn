{ mkDerivation, aeson, base, bytestring, errors, exceptions
, holborn-common-types, holborn-prelude, hspec-wai, hspec-wai-json
, http-api-data, http-client, http-types, optparse-applicative
, postgresql-simple, process, QuickCheck, servant-docs
, servant-server, stdenv, tasty, tasty-hspec, tasty-hunit
, tasty-quickcheck, text, time, transformers, unix, wai, wai-cors
, wai-extra, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring errors exceptions holborn-common-types
    holborn-prelude http-client http-types postgresql-simple
    servant-docs servant-server text time transformers wai warp
  ];
  executableHaskellDepends = [
    base holborn-common-types holborn-prelude optparse-applicative
    postgresql-simple wai-cors wai-extra warp
  ];
  testHaskellDepends = [
    aeson base bytestring errors holborn-common-types holborn-prelude
    hspec-wai hspec-wai-json http-api-data http-client http-types
    postgresql-simple process QuickCheck tasty tasty-hspec tasty-hunit
    tasty-quickcheck text transformers unix wai wai-extra
  ];
  description = "API server for Holborn";
  license = stdenv.lib.licenses.unfree;
}
