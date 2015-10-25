# Use Nix 15.09
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        language-python = self.callPackage ../nix/language-python.nix {};
      };
    };
in (modifiedHaskellPackages.callPackage ./. {}).env
