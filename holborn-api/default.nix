{ mkDerivation, aeson, attoparsec, base, basic-prelude, blaze-html
, blaze-markup, bytestring, either, envparse, errors
, holborn-common-types, http-client, http-reverse-proxy, http-types
, jwt, postgresql-simple, QuickCheck, servant-docs, servant-server
, stdenv, tasty, tasty-hunit, tasty-quickcheck, text, time
, transformers, wai, wai-cors, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude blaze-html blaze-markup
    bytestring either errors holborn-common-types http-client
    http-reverse-proxy http-types jwt postgresql-simple servant-docs
    servant-server text time transformers wai
  ];
  executableHaskellDepends = [
    base basic-prelude envparse holborn-common-types http-client
    postgresql-simple servant-server wai wai-cors warp
  ];
  testHaskellDepends = [
    base basic-prelude errors holborn-common-types postgresql-simple
    QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
