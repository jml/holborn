with (import <nixpkgs> {}).pkgs;
let
  autobahn = callPackage ./autobahn.nix { inherit txaio; };
  txaio = callPackage ./txaio.nix {};
in
rec {
  buildbot = plugins: callPackage ./buildbot-0.9.nix {
    inherit autobahn;
    # TODO: Actually, plugins should be dependencies of the configuration
    # package, not buildbot itself.
    plugins = plugins;
  };
  buildbot-pkg = callPackage ./buildbot-pkg-0.9.nix {};
  buildbot-waterfall-view = callPackage ./buildbot-waterfall-view-0.9.nix { inherit buildbot-pkg; };
  buildbot-worker = callPackage ./buildbot-worker-0.9.nix {};
  buildbot-www = callPackage ./buildbot-www-0.9.nix { inherit buildbot-pkg; buildbot = buildbot [];};
  oauth2_proxy = callPackage ./oauth2_proxy.nix {};
}
