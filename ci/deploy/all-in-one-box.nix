# A box running all the components of Buildbot
{ config, pkgs, lib, ... }:
let
  # Internal port the buildmaster listens on to hear from workers.
  workerPort = 9989;

  # Internal port listened on by the buildbot web service.
  buildbotWebPort = 3000;

  publicHostName = "buildbot.mumak.net";

  oauth2ProxyURL = "http://127.0.0.1:4180";

  # The public URL for buildbot.
  publicURL = "https://${publicHostName}";

  workerName = "single-host";

  # PUPPY: We don't really care what this is as long as master & worker are
  # synchronized.
  #
  # Secret randomly generated by Lastpass.
  workerPassword = "1HsPjuU9HvPRDVH58fNm3Sevy6666MILNDKh1O7V6jrUFxnL3v7Hb4EI93m3ax6o";

  configFile = pkgs.writeText "master.cfg" (import ./templates/master.cfg.nix
    { inherit workerPort buildbotWebPort;
      inherit workerName workerPassword;
      buildbotURL = "${publicURL}/";
      buildbotFromEmail = "holborn-buildbot@buildbot.mumak.net";
      projectName = "holborn";
      projectURL = "https://bitbucket.com/holbornlondon/holborn";
      # PUPPY: Re-using credentials from deploy box.
      gitRepo = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn";
      gitBranch = "master";
      builderName = "full-build";
      pollInterval = 300; # poll git repo every N seconds
    });

  challengeDir = "/var/www/challenges";

in
{
  require = [
    ../modules/buildbot-master.nix
    ../modules/buildbot-worker.nix
  ];

  environment.systemPackages = [];

  # Run GC at 8 am. If it breaks someone will be awake.
  nix.gc.automatic = true;
  nix.gc.dates = "08:00";
  nix.gc.options = "--delete-older-than 7d";


  # We need to build holborn, which is not free.
  nixpkgs.config.allowUnfree = true;

  # If the master doesn't come up properly then the worker will give up trying
  # to connect and need a kick in the pants.
  services.buildbot-worker = {
    enable = true;
    name = workerName;
    password  = workerPassword;
    adminContact = "Jonathan Lange <jml@mumak.net>";
    hostInfo = "Worker running on same box as master.";
    buildmasterHost = "localhost";
    buildmasterPort = workerPort;
    enableNixBuilds = true;
    allowUnfree = true;
    # Some of our tests run docker, so we need permission to run.
    extraGroups = [ "docker" ];
    extraPackages = [
      # We use make to run the tests
      pkgs.gnumake
      # We use direnv to make things work
      pkgs.direnv
      # direnv uses bash
      pkgs.bash
      # Test scripts use docker.
      pkgs.docker
    ];
  };

  # Needed by the worker to run certain tests.
  virtualisation.docker.enable = true;

  # TODO: Only bind web service to loopback device.
  services.buildbot = {
    inherit configFile;
    enable = true;
    # We need git to be able to poll for changes.
    extraPackages = [ pkgs.git ];
  };

  services.oauth2_proxy = {
    enable = true;
    provider = "google";
    clientID = "579594549675-nuh9d5m5kvdckfdec785o9cbcqe6sje7.apps.googleusercontent.com";
    package = (import ../packages).oauth2_proxy.bin // { outputs = [ "bin" ]; };
    # PUPPY: Shared secret.
    clientSecret = "bLlMleNsy_63ivGSIktA1EpP";
    cookie = {
      refresh = "1h";
      # XXX: jml doesn't actually understand what this is for.
      # PUPPY: Shared secret.
      # Randomly generated w/ Lastpass.
      secret = "dC1AKxDIlcDzmJOIUh0P1HG7CfoMke4u";
      secure = false;
    };
    httpAddress = oauth2ProxyURL;
    # oauth2_proxy *almost* guesses right. It insists on 'https' though, which
    # isn't being served (yet!) or configured as a valid redirect URL at
    # Google's side.
    redirectURL = "${publicURL}/oauth2/callback";
    upstream = "http://localhost:${toString buildbotWebPort}";
    email.addresses = ''
      jonathan.lange@gmail.com
      thomas.e.hunger@gmail.com
    '';
  };

  services.postfix = {
    enable = true;
  };

  security.acme.certs."buildbot.mumak.net" = {
    webroot = challengeDir;
    email = "jml@mumak.net";
  };

  services.nginx = {
    enable = true;
    config = (import ./templates/nginx.conf.nix {
      inherit challengeDir;
      serverName = publicHostName;
      backendURL = oauth2ProxyURL;
    });
  };

  services.sshd.enable = true;

  # We expose the 8000+ ports so that things run in docker containers (i.e.
  # SSH) can contact the docker host. Needed on worker only.
  networking.firewall.allowedTCPPorts = [ 22 80 443 8002 8080 8081 ];
}
