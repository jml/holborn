{ mkDerivation, aeson, async, base, basic-prelude, blaze-builder
, bytestring, either, envparse, http-types, lens-family-core, mtl
, network, network-simple, pipes, pipes-aeson, pipes-bytestring
, pipes-cliff, pipes-concurrency, pipes-network, pipes-parse
, pipes-safe, pipes-shell, pipes-zlib, process, servant
, servant-server, stdenv, stm, stm-chans, time, wai, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson async base basic-prelude blaze-builder bytestring either
    envparse http-types lens-family-core mtl network network-simple
    pipes pipes-aeson pipes-bytestring pipes-cliff pipes-concurrency
    pipes-network pipes-parse pipes-safe pipes-shell pipes-zlib process
    servant servant-server stm stm-chans time wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
