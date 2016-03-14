# Override haskell packages to pretend that our own libraries and
# binaries are part of standard hackage package set.
{ haskellPackages, haskell }:
haskellPackages.override {
    overrides = self: super: {

      # We're using `src = ./.` as the source attribute which
      # unfortunately includes everything in that directory including
      # ./dist which can lead to errors like "_o_split_User not
      # found". The best solution would be to filter the source
      # accordingly with builtins.filterSource but I could quite make
      # that work and still keep the "cabal2nix . > default.nix"
      # workflow. 2nd best solution is to delete ./dist before
      # compiling.
      mkDerivation = { pname, ... }@args:
        super.mkDerivation (if builtins.substring 0 7 pname == "holborn" then (args // { preCompileBuildDriver = "rm -rf ./dist";}) else args);

      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      holborn-common-types = self.callPackage ../holborn-common-types {};

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
