{ haskellPackages, haskell }:
haskellPackages.override {
    overrides = self: super: {
      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};

      aeson = haskell.lib.dontCheck (self.callPackage ./aeson.nix {});
      language-python = self.callPackage ./language-python.nix {};
      errors = self.callPackage ./errors.nix {};
      pipes-aeson = self.callPackage ./pipes-aeson.nix {};
      unexceptionalio = self.callPackage ./unexceptionalio.nix {};
    };
  }
