{ haskellPackages, haskell }:
haskellPackages.override {
    overrides = self: super: {
      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};

      # Temporary jailbreak until servant has been adjusted to include
      # aeson 0.11:
      servant-server = haskell.lib.doJailbreak super.servant-server;

      aeson = haskell.lib.dontCheck (self.callPackage ./aeson.nix {});
      language-python = self.callPackage ./language-python.nix {};
      errors = self.callPackage ./errors.nix {};
      pipes-aeson = haskell.lib.doJailbreak (self.callPackage ./pipes-aeson.nix {});
      unexceptionalio = self.callPackage ./unexceptionalio.nix {};

      purescript = haskell.lib.doJailbreak super.purescript;

      purescript-derive-lenses = self.callPackage ./purescript-derive-lenses.nix {};
    };
  }
