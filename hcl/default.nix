{ mkDerivation, base, basic-prelude, stdenv, turtle }:
mkDerivation {
  pname = "hcl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base basic-prelude turtle ];
  description = "Command line tool for holborn";
  license = stdenv.lib.licenses.unfree;
}
