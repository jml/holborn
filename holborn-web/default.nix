{ mkDerivation, base, basic-prelude, blaze-html, blaze-markup
, bytestring, containers, highlighter2, language-python, mtl
, pretty-show, servant, servant-blaze, servant-server, stdenv
, tasty, tasty-hunit, tasty-quickcheck, text, wai, warp
}:
mkDerivation {
  pname = "holborn-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-html blaze-markup bytestring containers
    highlighter2 language-python mtl pretty-show servant servant-blaze
    servant-server text wai warp
  ];
  testDepends = [
    base basic-prelude tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
