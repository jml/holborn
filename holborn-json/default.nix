{ mkDerivation, aeson, base, basic-prelude, bcrypt, bytestring
, containers, entropy, http-client, postgresql-simple, process
, servant, servant-server, stdenv, time, transformers
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base basic-prelude bcrypt bytestring containers entropy
    http-client postgresql-simple process servant servant-server time
    transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
