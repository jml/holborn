with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ./nix/all-packages.nix {};
in
(hp.mkDerivation {
  pname = "holborn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    hp.hcl
    hp.holborn-api
    hp.holborn-repo
    hp.holborn-ssh
    postgresql
  ];
  license = stdenv.lib.licenses.unfree;
}).env
