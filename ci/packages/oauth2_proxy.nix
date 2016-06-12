{ stdenv, buildGoPackage, fetchFromGitHub, pkgs }:

with pkgs.goPackages;

buildGoPackage {
  name = "oauth2_proxy";
  goPackagePath = "github.com/bitly/oauth2_proxy";
  # https://github.com/bitly/oauth2_proxy/pull/201/ adds web socket support,
  # which is necessary for buildbot's web UI.
  src = fetchFromGitHub {
    rev = "d67c0f62d9ebc856b198ba86aa02657660606ec6";
    owner = "soellman";
    repo = "oauth2_proxy";
    sha256 = "0khbpj2pbvi2hb9j5ks8fn267qby4hg0cxjz5wji4alms4l9n355";
  };

  goDeps = ./oauth2_proxy.deps.json;
}
