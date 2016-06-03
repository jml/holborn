# Override haskell packages to pretend that our own libraries and
# binaries are part of standard hackage package set.
{ haskellPackages, haskell, lib }:
let
  generateDirLocals = ''
    python ../tools/ghc_nix.py > .dir-locals.el
    echo "Regenerated .dir-locals.el to set flycheck GHC paths"
  '';
in
haskellPackages.override {
    overrides = self: super: {

      # We're using `src = ./.` as the source attribute which
      # unfortunately includes everything in that directory including
      # ./dist which can lead to errors like "_o_split_User not
      # found". The best solution would be to filter the source
      # accordingly with builtins.filterSource but Tom could not quite
      # make that work and still keep the "cabal2nix . > default.nix"
      # workflow. 2nd best solution is to delete ./dist before
      # compiling.
      mkDerivation = { pname, ... }@args:
        super.mkDerivation (
          if builtins.substring 0 7 pname == "holborn"
          then (args // { preCompileBuildDriver = "rm -rf ./dist";
                          shellHook = generateDirLocals; })
          else args
        );

      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      holborn-common-types = self.callPackage ../holborn-common-types {};

      # Switch to servant-0.5
      servant = super.servant_0_5;
      servant-blaze = super.servant-blaze_0_5;
      servant-docs = super.servant-docs_0_5;
      servant-server = super.servant-server_0_5;

      aeson = haskell.lib.dontCheck (self.callPackage ./aeson.nix {});
      language-python = self.callPackage ./language-python.nix {};
      pipes-aeson = haskell.lib.doJailbreak (self.callPackage ./pipes-aeson.nix {});
      unexceptionalio = self.callPackage ./unexceptionalio.nix {};

      # purescript relies on > 0.6 but we only have 0.5 branch in nix
      language-javascript = self.callPackage ./language-javascript.nix {};

      # jailbreak because of aeson, don't check because tests need npm
      purescript = haskell.lib.dontCheck (haskell.lib.doJailbreak (
        self.callPackage ./purescript.nix { language-javascript = self.language-javascript; }));
    };
}
