{ goPackages }:

goPackages.buildFromGitHub {
    rev    = "v0.4.0";
    owner  = "coreos";
    repo   = "dex";
    sha256 = "1cyf9zsn9v57rrr6q908adhmqa0la0xd62bhmia66s5mi2f9s76s";

    # dex comes with all its dependencies vendored
    preBuild = ''
      export GO15VENDOREXPERIMENT=1
    '';

    buildInputs = with goPackages; [ ldap asn1-ber text yaml-v1 viper tablewriter go-md2man ];
}
