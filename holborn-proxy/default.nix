{ mkDerivation, base, basic-prelude, bytestring, cookie, hoauth2
, http-client, http-reverse-proxy, http-types, jose, stdenv, text
, wai, warp, warp-tls
}:
mkDerivation {
  pname = "holborn-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base basic-prelude bytestring cookie hoauth2 http-client
    http-reverse-proxy http-types jose text wai warp warp-tls
  ];
  description = "Reverse proxy";
  license = stdenv.lib.licenses.unfree;
}
