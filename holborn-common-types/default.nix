{ mkDerivation, aeson, attoparsec, base, basic-prelude, bcrypt
, bytestring, containers, entropy, errors, http-api-data
, http-client, http-media, postgresql-simple, process, QuickCheck
, servant, servant-server, stdenv, tasty, tasty-hunit
, tasty-quickcheck, time, lib
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude bcrypt bytestring containers
    entropy errors http-api-data http-client http-media
    postgresql-simple process QuickCheck servant servant-server time
  ];
  testHaskellDepends = [
    aeson base basic-prelude tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.unfree;
}
