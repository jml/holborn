# Override haskell packages to pretend that our own libraries and
# binaries are part of standard hackage package set.
{ haskellPackages, haskell }:
haskellPackages.override {
    overrides = self: super: {
      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      holborn-json = self.callPackage ../holborn-json {};

      # Temporary jailbreak until servant has been adjusted to include
      # aeson 0.11:
      servant-server = haskell.lib.doJailbreak super.servant-server;

      aeson = haskell.lib.dontCheck (self.callPackage ./aeson.nix {});
      language-python = self.callPackage ./language-python.nix {};
      pipes-aeson = haskell.lib.doJailbreak (self.callPackage ./pipes-aeson.nix {});
      unexceptionalio = self.callPackage ./unexceptionalio.nix {};

      purescript = haskell.lib.doJailbreak super.purescript;
    };
}
