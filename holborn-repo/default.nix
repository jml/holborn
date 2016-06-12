{ mkDerivation, aeson, base, blaze-builder, blaze-html
, blaze-markup, bytestring, conduit, conduit-combinators, directory
, envparse, errors, gitlib, gitlib-libgit2, holborn-common-types
, holborn-prelude, holborn-syntax, http-api-data, http-types, mtl
, network, pipes, pipes-aeson, pipes-bytestring, pipes-network
, pipes-parse, pipes-safe, pipes-shell, pipes-zlib, process
, servant-blaze, servant-server, stdenv, tagged, text, time
, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-builder blaze-html blaze-markup bytestring conduit
    conduit-combinators directory errors gitlib gitlib-libgit2
    holborn-common-types holborn-prelude holborn-syntax http-api-data
    http-types mtl network pipes pipes-aeson pipes-bytestring
    pipes-network pipes-parse pipes-safe pipes-shell pipes-zlib process
    servant-blaze servant-server tagged text transformers wai warp
  ];
  executableHaskellDepends = [
    base envparse holborn-prelude servant-server time wai wai-extra
    warp
  ];
  license = stdenv.lib.licenses.unfree;
}
