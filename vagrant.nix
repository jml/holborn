{config, pkgs, ...}:
with pkgs;
let t = writeText "local-nixconf.conf" ''
  { allowUnfree = true; }
''; in
{
  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = [
    direnv
    nix-repl
    cabal-install
    postgresql
    git
    tmux
    docker
    gnumake
    # ghc?
    # open-haddock?
    # nox?
    # stuff for purescript
  ];

  # Allow default pulp server, default holborn-api, and default holborn-repo.
  #
  # The SSH docker image won't be able to reach holborn-api and holborn-repo
  # on the main host if these aren't enabled.
  networking.firewall.allowedTCPPorts = [ 1337 8002 8080 8081 ];

  # Assumes bash, which is slightly wrong, since this variable is used in all
  # shells.
  environment.interactiveShellInit = ''
    eval "$(${direnv}/bin/direnv hook bash)"
    direnv allow /vagrant
    mkdir -p ~/.nixpkgs
    cp ${t} ~/.nixpkgs/config.nix
  '';

  virtualisation.docker.enable = true;

  users.extraUsers.vagrant = {
    extraGroups = [ "docker" ];
  };
}
