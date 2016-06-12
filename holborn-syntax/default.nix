{ mkDerivation, base, bytestring, containers, errors, highlighter2
, holborn-prelude, language-python, mtl, pretty-error, stdenv
, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "holborn-syntax";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers errors highlighter2 holborn-prelude
    language-python mtl pretty-error text
  ];
  testHaskellDepends = [
    base holborn-prelude tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://bitbucket.com/mumak/holborn";
  description = "Syntax analysis library";
  license = stdenv.lib.licenses.unfree;
}
