{ mkDerivation, base, basic-prelude, blaze-html, blaze-markup
, bytestring, containers, errors, highlighter2, language-python
, mtl, pretty-error, servant-server, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, wai
}:
mkDerivation {
  pname = "holborn-syntax";
  version = "0.2.0.0";
  src = ./.;
  buildDepends = [
    base basic-prelude blaze-html blaze-markup bytestring containers
    errors highlighter2 language-python mtl pretty-error servant-server
    text wai
  ];
  testDepends = [
    base basic-prelude tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://bitbucket.com/mumak/holborn";
  description = "Syntax analysis library";
  license = stdenv.lib.licenses.unfree;
}
