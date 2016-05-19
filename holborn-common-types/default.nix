{ mkDerivation, aeson, attoparsec, base, basic-prelude, bcrypt
, bytestring, containers, entropy, errors, http-api-data
, http-client, http-media, postgresql-simple, process, servant
, servant-server, stdenv, tasty, tasty-quickcheck, time
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude bcrypt bytestring containers
    entropy errors http-api-data http-client http-media
    postgresql-simple process servant servant-server time
  ];
  testHaskellDepends = [ base basic-prelude tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.unfree;
}
