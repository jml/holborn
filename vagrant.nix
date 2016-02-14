{config, pkgs, ...}:
with pkgs;
let t = writeText "local-nixconf.conf" ''
  { allowUnfree = true; }
''; in
{
  nixpkgs.config = {
    # Doesn't seem to allow me to use holborn-syntax. Is there actually a way
    # to do this?
    allowUnfree = true;
  };

  environment.systemPackages = [
    direnv
    nix-repl
    cabal-install
    postgresql
    git
    # ghc?
    # open-haddock?
    # nox?
    # stuff for purescript
  ];

  # Allow default pulp server.
  networking.firewall.allowedTCPPorts = [ 1337 ];

  # Assumes bash, which is slightly wrong, since this variable is used in all
  # shells.
  environment.interactiveShellInit = ''
    eval "$(${direnv}/bin/direnv hook bash)"
    direnv allow /vagrant
    mkdir -p ~/.nixpkgs
    cp ${t} ~/.nixpkgs/config.nix
  '';

}
