with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
in
(hp.callPackage ./. {}).env // {
  buildDepends = [
    pkgs.pythonPackages.sqlalchemy_1_0
    pkgs.pythonPackages.networkx
    pkgs.pythonPackages.psycopg2
    pkgs.pythonPackages.pydot
    pkgs.graphviz
  ];
}
