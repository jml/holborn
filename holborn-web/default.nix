{ mkDerivation, base, basic-prelude, highlighter2, Spock, stdenv }:
mkDerivation {
  pname = "holborn-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base basic-prelude highlighter2 Spock ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
