{ mkDerivation, base, basic-prelude, bytestring, containers, errors
, highlighter2, language-python, mtl, pretty-error, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "holborn-syntax";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [
    base basic-prelude bytestring containers errors highlighter2
    language-python mtl pretty-error text
  ];
  testDepends = [
    base basic-prelude tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://bitbucket.com/mumak/holborn";
  description = "Syntax analysis library";
  license = stdenv.lib.licenses.unfree;
}
