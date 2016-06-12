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
          then (args // { src = lib.sourceFilesBySuffices args.src [".cabal" ".hs"];
                          shellHook = generateDirLocals; })
          else args
        );

      hcl = self.callPackage ../hcl {};
      holborn-api = self.callPackage ../holborn-api {};
      holborn-prelude = self.callPackage ../holborn-prelude {};
      holborn-repo = self.callPackage ../holborn-repo {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      holborn-common-types = self.callPackage ../holborn-common-types {};

      language-python = self.callPackage ./language-python.nix {};
      unexceptionalio = self.callPackage ./unexceptionalio.nix {};

      # purescript relies on > 0.6 but we only have 0.5 branch in nix
      language-javascript = self.callPackage ./language-javascript.nix {};

      # Get pipes-shell working with ghc 8.0
      pipes-shell = self.callPackage ./pipes-shell.nix {};

      # Don't check because tests need phantomjs
      wai-cors = haskell.lib.dontCheck haskellPackages.wai-cors;
      # Don't check because tests need nodejs
      purescript = haskell.lib.dontCheck (self.callPackage ./purescript.nix {});
    };
}
