{ mkDerivation, base, basic-prelude, blaze-html, blaze-markup
, bytestring, containers, directory, either, envparse, errors
, filepath, highlighter2, language-python, mtl, pretty-error
, servant, servant-blaze, servant-server, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, wai, warp
}:
mkDerivation {
  pname = "holborn-syntax";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-html blaze-markup bytestring containers
    directory either envparse errors filepath highlighter2
    language-python mtl pretty-error servant servant-blaze
    servant-server text wai warp
  ];
  testDepends = [
    base basic-prelude tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://bitbucket.com/mumak/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.unfree;
}
