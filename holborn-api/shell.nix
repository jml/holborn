with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
  p = (hp.callPackage ./. {});
in
pkgs.stdenv.lib.overrideDerivation p.env (oldAttrs: {
  # Graph stuff isn't strictly necessary and a bit of a pain to build on Darwin.
  buildInputs = if stdenv.isDarwin then oldAttrs.buildInputs else oldAttrs.buildInputs ++ [
    pkgs.pythonPackages.sqlalchemy
    pkgs.pythonPackages.networkx
    pkgs.pythonPackages.psycopg2
    pkgs.pythonPackages.pydot
    pkgs.graphviz
  ];
})
