{ mkDerivation, aeson, aeson-better-errors, ansi-terminal
, ansi-wl-pprint, base, base-compat, bower-json, boxes, bytestring
, containers, directory, dlist, edit-distance, filepath, fsnotify
, Glob, haskeline, hspec, hspec-discover, http-types, HUnit
, language-javascript, lifted-base, monad-control, monad-logger
, mtl, network, optparse-applicative, parallel, parsec
, pattern-arrows, pipes, pipes-http, process, regex-tdfa, safe
, semigroups, silently, sourcemap, spdx, split, stdenv, stm, syb
, text, time, transformers, transformers-base, transformers-compat
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "purescript";
  version = "0.9.1";
  sha256 = "1ryhlpcxxl5b1f3py0vkkl43m956i9xvx181cp16wssxi8ack93j";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors ansi-terminal base base-compat bower-json
    boxes bytestring containers directory dlist edit-distance filepath
    fsnotify Glob haskeline http-types language-javascript lifted-base
    monad-control monad-logger mtl parallel parsec pattern-arrows pipes
    pipes-http process regex-tdfa safe semigroups sourcemap spdx split
    stm syb text time transformers transformers-base
    transformers-compat unordered-containers utf8-string vector
  ];
  executableHaskellDepends = [
    aeson ansi-terminal ansi-wl-pprint base base-compat boxes
    bytestring containers directory filepath Glob haskeline
    monad-logger mtl network optparse-applicative parsec process split
    stm text time transformers transformers-compat utf8-string
  ];
  testHaskellDepends = [
    aeson aeson-better-errors base base-compat boxes bytestring
    containers directory filepath Glob haskeline hspec hspec-discover
    HUnit mtl optparse-applicative parsec process silently stm text
    time transformers transformers-compat utf8-string vector
  ];
  homepage = "http://www.purescript.org/";
  description = "PureScript Programming Language Compiler";
  license = stdenv.lib.licenses.mit;
}
