{ mkDerivation, base, basic-prelude, Spock, stdenv }:
mkDerivation {
  pname = "holborn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base basic-prelude Spock ];
  homepage = "https://github.com/jml/holborn";
  description = "Code search";
  license = stdenv.lib.licenses.gpl3;
}
