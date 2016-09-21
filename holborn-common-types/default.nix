{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, holborn-prelude, http-api-data, http-media, postgresql-simple
, process, QuickCheck, servant-server, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers holborn-prelude
    http-api-data http-media postgresql-simple process QuickCheck
    servant-server text time
  ];
  testHaskellDepends = [
    aeson base holborn-prelude http-api-data tasty tasty-hunit
    tasty-quickcheck
  ];
  description = "Types shared between our nodes";
  license = stdenv.lib.licenses.unfree;
}
