{ mkDerivation, base, basic-prelude, blaze-html, highlighter2
, Spock, stdenv, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "holborn-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-html highlighter2 Spock text
  ];
  testDepends = [ base basic-prelude tasty tasty-hunit ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
