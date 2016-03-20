{ mkDerivation, base, basic-prelude, network-simple, stdenv, turtle
}:
mkDerivation {
  pname = "hcl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base basic-prelude network-simple turtle
  ];
  description = "Command line tool for holborn";
  license = stdenv.lib.licenses.unfree;
}
