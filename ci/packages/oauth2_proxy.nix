{ stdenv, buildGoPackage, fetchFromGitHub, pkgs }:

with pkgs.goPackages;

buildGoPackage {
  name = "oauth2_proxy";
  goPackagePath = "github.com/bitly/oauth2_proxy";
  src = fetchFromGitHub {
    rev = "10f47e325b782a60b8689653fa45360dee7fbf34";
    owner = "bitly";
    repo = "oauth2_proxy";
    sha256 = "13f6kaq15f6ial9gqzrsx7i94jhd5j70js2k93qwxcw1vkh1b6si";
  };
  buildInputs = [
    go-assert go-options go-simplejson toml fsnotify.v1 oauth2
    google-api-go-client hmacauth
  ];
}
