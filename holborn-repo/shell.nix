with (import <nixpkgs> {}).pkgs;
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
