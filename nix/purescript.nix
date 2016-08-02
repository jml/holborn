{ mkDerivation, aeson, aeson-better-errors, ansi-terminal
, ansi-wl-pprint, base, base-compat, bower-json, boxes, bytestring
, clock, containers, directory, dlist, edit-distance, file-embed
, filepath, fsnotify, Glob, haskeline, hspec, hspec-discover
, http-client, http-types, HUnit, language-javascript, lifted-base
, monad-control, monad-logger, mtl, network, optparse-applicative
, parallel, parsec, pattern-arrows, pipes, pipes-http, process
, protolude, regex-tdfa, safe, semigroups, silently, sourcemap
, spdx, split, stdenv, stm, syb, text, time, transformers
, transformers-base, transformers-compat, unordered-containers
, utf8-string, vector, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "purescript";
  version = "0.9.3";
  sha256 = "1rci7friyz44nc5ipflb72pmhlimd37gqgrx21l7b8q84lijhihf";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors ansi-terminal base base-compat bower-json
    boxes bytestring clock containers directory dlist edit-distance
    filepath fsnotify Glob haskeline http-client http-types
    language-javascript lifted-base monad-control monad-logger mtl
    parallel parsec pattern-arrows pipes pipes-http process protolude
    regex-tdfa safe semigroups sourcemap spdx split stm syb text time
    transformers transformers-base transformers-compat
    unordered-containers utf8-string vector
  ];
  executableHaskellDepends = [
    aeson ansi-terminal ansi-wl-pprint base base-compat boxes
    bytestring containers directory file-embed filepath Glob haskeline
    http-types monad-logger mtl network optparse-applicative parsec
    process protolude split stm text time transformers
    transformers-compat utf8-string wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson aeson-better-errors base base-compat boxes bytestring
    containers directory filepath Glob haskeline hspec hspec-discover
    HUnit mtl optparse-applicative parsec process protolude silently
    stm text time transformers transformers-compat utf8-string vector
  ];
  homepage = "http://www.purescript.org/";
  description = "PureScript Programming Language Compiler";
  license = stdenv.lib.licenses.mit;
}
