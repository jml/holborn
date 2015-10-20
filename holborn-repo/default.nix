{ mkDerivation, base, basic-prelude, blaze-builder, bytestring
, either, envparse, http-types, pipes, pipes-safe, pipes-shell
, pipes-zlib, servant, servant-server, stdenv, time, wai, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-builder bytestring either envparse
    http-types pipes pipes-safe pipes-shell pipes-zlib servant
    servant-server time wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
