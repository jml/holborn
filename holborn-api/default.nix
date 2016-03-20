{ mkDerivation, aeson, attoparsec, base, basic-prelude, blaze-html
, bytestring, containers, either, entropy, envparse, errors
, holborn-common-types, http-client, http-reverse-proxy, http-types
, jwt, postgresql-simple, process, QuickCheck, servant
, servant-blaze, servant-docs, servant-server, shakespeare, stdenv
, tasty, tasty-hunit, tasty-quickcheck, text, time, transformers
, wai, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude blaze-html bytestring
    containers either entropy errors holborn-common-types http-client
    http-reverse-proxy http-types jwt postgresql-simple process servant
    servant-blaze servant-docs servant-server shakespeare text time
    transformers wai
  ];
  executableHaskellDepends = [
    base basic-prelude blaze-html bytestring envparse
    holborn-common-types http-client postgresql-simple servant-server
    wai warp
  ];
  testHaskellDepends = [
    base basic-prelude errors holborn-common-types postgresql-simple
    QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
