{ goPackages }:

goPackages.buildFromGitHub {
    rev    = "v0.4.0";
    owner  = "coreos";
    repo   = "dex";
    sha256 = "1cyf9zsn9v57rrr6q908adhmqa0la0xd62bhmia66s5mi2f9s76s";
}
