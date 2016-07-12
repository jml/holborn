{ mkDerivation, aeson, base, base64-bytestring, bytestring, cookie
, entropy, hashable, hoauth2, holborn-prelude, http-client
, http-reverse-proxy, http-types, jose, optparse-applicative
, servant, servant-server, stdenv, stm, text, transformers
, unordered-containers, wai, wai-extra, warp, warp-tls
}:
mkDerivation {
  pname = "holborn-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring cookie entropy hashable
    hoauth2 holborn-prelude http-client http-reverse-proxy http-types
    jose servant servant-server stm text transformers
    unordered-containers wai warp
  ];
  executableHaskellDepends = [
    base holborn-prelude http-client optparse-applicative wai-extra
    warp warp-tls
  ];
  description = "Reverse proxy";
  license = stdenv.lib.licenses.unfree;
}
