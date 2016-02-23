{ mkDerivation, aeson, attoparsec, base, basic-prelude, bcrypt
, blaze-html, bytestring, containers, either, envparse, errors, jwt
, postgresql-simple, QuickCheck, servant, servant-blaze
, servant-docs, servant-server, shakespeare, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, time, transformers, wai
, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude bcrypt blaze-html bytestring
    containers either errors jwt postgresql-simple servant
    servant-blaze servant-docs servant-server shakespeare text time
    transformers
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
