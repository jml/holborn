# Use Nix 15.09
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
let
  hp = haskellPackages.override {
    overrides = self: super: {
    };
  };
in
(hp.callPackage ./. {}).env
