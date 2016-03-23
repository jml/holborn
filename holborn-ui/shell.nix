with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
in
(callPackage ./. { haskellPackages = hp;})
