{ mkDerivation, aeson, base, base64-bytestring, basic-prelude
, bytestring, cookie, entropy, hashable, hoauth2, http-client
, http-reverse-proxy, http-types, jose, optparse-applicative
, servant, servant-server, stdenv, stm, text, transformers
, unordered-containers, wai, warp, warp-tls
}:
mkDerivation {
  pname = "holborn-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base basic-prelude entropy hashable hoauth2 optparse-applicative
    stm unordered-containers warp
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring basic-prelude bytestring cookie
    hoauth2 http-client http-reverse-proxy http-types jose servant
    servant-server text transformers unordered-containers wai warp
    warp-tls
  ];
  description = "Reverse proxy";
  license = stdenv.lib.licenses.unfree;
}
