{ mkDerivation, aeson, base, blaze-builder, blaze-html
, blaze-markup, bytestring, conduit, conduit-combinators
, containers, directory, errors, gitlib, gitlib-libgit2
, holborn-common-types, holborn-prelude, holborn-syntax
, http-api-data, http-types, megaparsec, mtl, network
, optparse-applicative, pipes, pipes-aeson, pipes-bytestring
, pipes-network, pipes-parse, pipes-safe, pipes-shell, pipes-zlib
, process, QuickCheck, servant-blaze, servant-server, stdenv
, tagged, text, time, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-builder blaze-html blaze-markup bytestring conduit
    conduit-combinators containers directory errors gitlib
    gitlib-libgit2 holborn-common-types holborn-prelude holborn-syntax
    http-api-data http-types megaparsec mtl network pipes pipes-aeson
    pipes-bytestring pipes-network pipes-parse pipes-safe pipes-shell
    pipes-zlib process QuickCheck servant-blaze servant-server tagged
    text transformers wai warp
  ];
  executableHaskellDepends = [
    base holborn-prelude optparse-applicative servant-server time wai
    wai-extra warp
  ];
  description = "Serve git repositories";
  license = stdenv.lib.licenses.unfree;
}
