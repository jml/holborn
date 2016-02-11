{ mkDerivation, aeson, attoparsec, base, basic-prelude, bcrypt
, blaze-html, bytestring, containers, either, envparse, errors, jwt
, postgresql-simple, QuickCheck, servant, servant-blaze
, servant-server, shakespeare, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time, wai, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base basic-prelude bcrypt blaze-html bytestring
    containers either envparse errors jwt postgresql-simple servant
    servant-blaze servant-server shakespeare text time wai warp
  ];
  testDepends = [
    base basic-prelude errors postgresql-simple QuickCheck tasty
    tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
