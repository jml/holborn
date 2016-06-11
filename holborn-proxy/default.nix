{ mkDerivation, aeson, base, basic-prelude, bytestring, cookie
, envparse, hashable, hoauth2, http-client, http-reverse-proxy
, http-types, jose, stdenv, stm, text, unordered-containers, wai
, warp, warp-tls
}:
mkDerivation {
  pname = "holborn-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    aeson base basic-prelude bytestring cookie envparse hashable
    hoauth2 http-client http-reverse-proxy http-types jose stm text
    unordered-containers wai warp warp-tls
  ];
  description = "Reverse proxy";
  license = stdenv.lib.licenses.unfree;
}
