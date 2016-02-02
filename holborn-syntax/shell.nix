with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        language-python = self.callPackage ../nix/language-python.nix {};
      };
    };
in (modifiedHaskellPackages.callPackage ./. {}).env
