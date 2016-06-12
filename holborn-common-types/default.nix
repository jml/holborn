{ mkDerivation, aeson, attoparsec, base, bcrypt, bytestring
, containers, entropy, errors, holborn-prelude, http-api-data
, http-client, http-media, postgresql-simple, process, QuickCheck
, servant, servant-server, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bcrypt bytestring containers entropy errors
    holborn-prelude http-api-data http-client http-media
    postgresql-simple process QuickCheck servant servant-server text
    time
  ];
  testHaskellDepends = [
    aeson base holborn-prelude tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
