{ mkDerivation, aeson, async, base, basic-prelude, blaze-builder
, bytestring, either, envparse, http-types, mtl, network
, network-simple, pipes, pipes-aeson, pipes-bytestring
, pipes-concurrency, pipes-network, pipes-parse, pipes-safe
, pipes-shell, pipes-zlib, process, process-streaming, servant
, servant-server, stdenv, text, time, wai, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson async base basic-prelude blaze-builder bytestring either
    envparse http-types mtl network network-simple pipes pipes-aeson
    pipes-bytestring pipes-concurrency pipes-network pipes-parse
    pipes-safe pipes-shell pipes-zlib process process-streaming servant
    servant-server text time wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
