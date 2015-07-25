{ mkDerivation, base, basic-prelude, blaze-html, highlighting-kate
, Spock, stdenv, tasty
}:
mkDerivation {
  pname = "holborn-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-html highlighting-kate Spock
  ];
  testDepends = [ base tasty ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
