with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
  p = (hp.callPackage ./. {});
in
pkgs.stdenv.lib.overrideDerivation p.env (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ [
    pkgs.pythonPackages.sqlalchemy_1_0
    pkgs.pythonPackages.networkx
    pkgs.pythonPackages.psycopg2
    pkgs.pythonPackages.pydot
    pkgs.graphviz
  ];
})
