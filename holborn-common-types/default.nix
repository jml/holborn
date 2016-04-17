{ mkDerivation, aeson, base, basic-prelude, bcrypt, bytestring
, containers, entropy, http-api-data, http-client
, postgresql-simple, process, servant, servant-server, stdenv, time
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base basic-prelude bcrypt bytestring containers entropy
    http-api-data http-client postgresql-simple process servant
    servant-server time
  ];
  license = stdenv.lib.licenses.unfree;
}
