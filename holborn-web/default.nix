{ mkDerivation, base, basic-prelude, blaze-html, highlighting-kate
, Spock, stdenv
}:
mkDerivation {
  pname = "holborn-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base basic-prelude blaze-html highlighting-kate Spock
  ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
