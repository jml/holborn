{ mkDerivation, aeson, base, basic-prelude, bcrypt, bytestring
, containers, entropy, http-api-data, http-client
, postgresql-simple, process, stdenv, time, transformers
}:
mkDerivation {
  pname = "holborn-common-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base basic-prelude bcrypt bytestring containers entropy
    http-api-data http-client postgresql-simple process time
    transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
