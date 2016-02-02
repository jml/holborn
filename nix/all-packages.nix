{ haskellPackages }:

haskellPackages.override {
    overrides = self: super: {
      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      language-python = self.callPackage ../nix/language-python.nix {};
      errors = self.callPackage ../nix/errors.nix {};
      unexceptionalio = self.callPackage ../nix/unexceptionalio.nix {};
    };
  }
