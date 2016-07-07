{ buildGo16Package, fetchFromGitHub }:
buildGo16Package rec {
  version    = "v0.4.0";
  name = "dex-${version}";
  goPackagePath = "github.com/coreos/dex";

  src = fetchFromGitHub {
      rev    = version;
      owner  = "coreos";
      repo   = "dex";
      sha256 = "1cyf9zsn9v57rrr6q908adhmqa0la0xd62bhmia66s5mi2f9s76s";
  };

  #goDeps = ./dex-vendor.json;
}
