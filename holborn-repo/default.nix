{ mkDerivation, aeson, async, base, basic-prelude, blaze-builder
, blaze-html, blaze-markup, bytestring, either, envparse, errors
, gitlib, gitlib-libgit2, holborn-syntax, http-types, mtl, network
, network-simple, pipes, pipes-aeson, pipes-bytestring
, pipes-network, pipes-parse, pipes-safe, pipes-shell, pipes-zlib
, process, servant, servant-blaze, servant-server, stdenv, tagged
, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson async base basic-prelude blaze-builder blaze-html
    blaze-markup bytestring either envparse errors gitlib
    gitlib-libgit2 holborn-syntax http-types mtl network network-simple
    pipes pipes-aeson pipes-bytestring pipes-network pipes-parse
    pipes-safe pipes-shell pipes-zlib process servant servant-blaze
    servant-server tagged text time transformers wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
