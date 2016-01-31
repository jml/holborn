# Use Nix 15.09
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
let
  hp = haskellPackages.override {
    overrides = self: super: {
      holborn-syntax = self.callPackage ../holborn-syntax {};
      language-python = self.callPackage ../nix/language-python.nix {};
      errors = self.callPackage ../nix/errors.nix {};
      unexceptionalio = self.callPackage ../nix/unexceptionalio.nix {};
    };
  };
in
(hp.callPackage ./. {}).env
