{ mkDerivation, aeson, attoparsec, base, basic-prelude, bcrypt
, blaze-html, bytestring, containers, either, entropy, envparse
, errors, http-reverse-proxy, jwt, postgresql-simple, process
, QuickCheck, servant, servant-blaze, servant-docs, servant-server
, shakespeare, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, time, transformers, wai, warp, holborn-json
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude bcrypt blaze-html bytestring
    containers either entropy errors http-reverse-proxy jwt
    postgresql-simple process servant servant-blaze servant-docs
    servant-server shakespeare text time transformers wai
    holborn-json
  ];
  executableHaskellDepends = [
    base basic-prelude blaze-html bytestring envparse postgresql-simple
    servant-server wai warp
  ];
  testHaskellDepends = [
    base basic-prelude errors postgresql-simple QuickCheck tasty
    tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
