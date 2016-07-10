{ mkDerivation, base, basic-prelude, envparse, holborn-api
, holborn-common-types, holborn-prelude, http-client
, optparse-applicative, pipes, pipes-aeson, pipes-bytestring
, pipes-network, servant, servant-client, stdenv, transformers
}:
mkDerivation {
  pname = "holborn-ssh";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base basic-prelude envparse holborn-api holborn-common-types
    holborn-prelude http-client optparse-applicative pipes pipes-aeson
    pipes-bytestring pipes-network servant servant-client transformers
  ];
  license = stdenv.lib.licenses.unfree;
  enableSharedExecutables = false;
}
