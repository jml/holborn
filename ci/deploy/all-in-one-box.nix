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
      projectName = "holborn";
      projectURL = "https://bitbucket.com/holbornlondon/holborn";
      # PUPPY: Re-using credentials from deploy box.
      gitRepo = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn";
      gitBranch = "master";
      builderName = "holborn-experimental-builder";
      pollInterval = 300; # poll git repo every N seconds
    });

  challengeDir = "/var/www/challenges";

in
{
  require = [
    ../modules/buildbot-master.nix
    ../modules/buildbot-worker.nix
    ../modules/oauth2_proxy.nix
  ];

  environment.systemPackages = [];

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
    extraPackages = [
      # We need git to be able to get the source code to build it!
      pkgs.git
    ];
  };

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
    package = (pkgs.callPackage ../packages/oauth2_proxy.nix {
      buildGoPackage = pkgs.goPackages.buildGoPackage;
    }).bin // { outputs = [ "bin" ]; };
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
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];
}