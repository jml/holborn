# Override haskell packages to pretend that our own libraries and
# binaries are part of standard hackage package set.
{ haskellPackages, haskell, lib, stdenv, fetchFromGitHub, pkgs }:
let
  capture-all-fork = fetchFromGitHub {
    owner = "jml";
    repo = "servant";
    rev = "capture-all-fork";
    sha256 = "05z51jjpzwav1nrsh8xc0mwbk105k9x89zq9ii2rwzbhrk0gn53z";
  };

  # These helpers really ought to be in standard haskell lib:
  # https://github.com/NixOS/nixpkgs/issues/17226
  addRuntimeDependency = drv: x: addRuntimeDependencies drv [x];
  addRuntimeDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      ${drv.postInstall or ""}

      for exe in "$out/bin/"* ; do
      wrapProgram "$exe" --prefix PATH ":" \
          ${lib.makeBinPath xs}
      done
    '';
  });

  addTestDependency = drv: x: addTestDependencies drv [x];
  addTestDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    testToolDepends = xs;
  });

  addLibrarySystemDependency = drv: x: addLibrarySystemDependencies drv [x];
  addLibrarySystemDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    librarySystemDepends = xs ;
  });

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
          then (args // {
            src = lib.sourceFilesBySuffices args.src [".cabal" ".hs" ".sql"];
            # When we put our binaries into Docker images, they make huge
            # docker images (2GB+) unless we set this option.
            enableSharedExecutables = false;
          })
          else args
        );

      hcl = self.callPackage ../hcl {};
      # holborn-api tests have a runtime dependency on postgresql.
      # holborn-api calls 'ssh-keygen' during normal operation and during tests.
      holborn-api = addLibrarySystemDependency (
          addTestDependencies (
            addRuntimeDependency (self.callPackage ../holborn-api {}) pkgs.openssh)
          [pkgs.postgresql pkgs.openssh])
        pkgs.postgresql.lib;
      # holborn-common-types calls 'ssh-keygen' in its tests
      holborn-common-types = addTestDependency (self.callPackage ../holborn-common-types {}) pkgs.openssh;
      holborn-prelude = self.callPackage ../holborn-prelude {};
      # holborn-repo calls 'git' during normal operation.
      holborn-repo = addRuntimeDependency (self.callPackage ../holborn-repo {}) pkgs.git;
      holborn-ssh = self.callPackage ../holborn-ssh {};
      holborn-syntax = self.callPackage ../holborn-syntax {};
      holborn-proxy = self.callPackage ../holborn-proxy {};

      unexceptionalio = self.callPackage ./unexceptionalio.nix {};

      # Patched hoauth2 package with id_token field: Drop in favour of
      # main package when https://github.com/freizl/hoauth2/pull/48 is
      # in.
      hoauth2 = self.callPackage ./hoauth2.nix {};

      # purescript relies on > 0.6 but we only have 0.5 branch in nix
      language-javascript = self.callPackage ./language-javascript.nix {};

      # Get pipes-shell working with ghc 8.0
      pipes-shell = self.callPackage ./pipes-shell.nix {};

      # We use a fork of Servant that has CaptureAll
      servant = self.callPackage ./servant/servant.nix { forked-servant = capture-all-fork; };
      servant-client = self.callPackage ./servant/servant-client.nix { forked-servant = capture-all-fork; };
      servant-docs = self.callPackage ./servant/servant-docs.nix { forked-servant = capture-all-fork; };
      servant-foreign = self.callPackage ./servant/servant-foreign.nix { forked-servant = capture-all-fork; };
      servant-js = self.callPackage ./servant/servant-js.nix { forked-servant = capture-all-fork; };
      servant-mock = self.callPackage ./servant/servant-mock.nix { forked-servant = capture-all-fork; };
      servant-server = self.callPackage ./servant/servant-server.nix { forked-servant = capture-all-fork; };

      # Don't check because tests need phantomjs
      wai-cors = haskell.lib.dontCheck haskellPackages.wai-cors;

      # purescript needs http-client <0.5 and protolude > 0.1.6 ATM
      # re-check http-client after next purescript version and protolude after next nixpkgs HEAD update
      # See: https://github.com/purescript/purescript/issues/2237
      http-client = self.callPackage ./http-client.nix {};
      protolude = self.callPackage ./protolude.nix {};

      # Don't check because tests need nodejs
      purescript = haskell.lib.dontCheck (self.callPackage ./purescript.nix {});

      # not yet in the nix package repo, remove when it's there. broken tests
      servant-subscriber = haskell.lib.dontCheck (self.callPackage ./servant-subscriber.nix {});
      # not yet in the nix package repo, remove when it's there:
      servant-purescript = self.callPackage ./servant-purescript.nix {};
    };
}
