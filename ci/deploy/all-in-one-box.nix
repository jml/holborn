# A box running all the components of Buildbot
{ config, pkgs, lib, ... }:
let
  workerPort = 9989;
  # XXX: PUPPY: Re-using credentials from deploy box.
  # TODO: Find out how Tom generated this key. Looks like bitbucket would
  # rather we gain read-only access with a "deployment key" (an SSH key)
  gitRepo = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn";
  gitBranch = "master";
  builderName = "holborn-experimental-builder";
  projectName = "holborn";
  projectURL = "https://bitbucket.com/holbornlondon/holborn";
  buildbotWebPort = 3000;
  # XXX: Duplicated somewhat from the AWS deployment file:
  # XXX: Port is from oauth2_proxy's httpAddress
  buildbotURL = "http://buildbot.mumak.net:4180/";
  # TODO: Change this to 5 minutes once we're done configuring the server.
  pollInterval = 60; # poll git repo every N seconds

  workerName = "single-host";
  # XXX: PUPPY: We don't really care what this is as long as master & worker
  # are synchronized. Can we ask Nix to generate it?
  workerPassword = "whatever";

in
{
  require = [
    ../modules/buildbot-master.nix
    ../modules/buildbot-worker.nix
    ../modules/oauth2_proxy.nix
  ];

  environment.systemPackages = [];

  # XXX: If the master doesn't come up properly then the worker will give up
  # trying to connect and need a kick in the pants. Not sure how to properly
  # encode this, since in the general case the worker will be running on a
  # different machine.
  services.buildbot-worker = {
    enable = true;
    name = workerName;
    password  = workerPassword;
    adminContact = "Jonathan Lange <jml@mumak.net>";
    hostInfo = "Worker running on same box as master.";
    buildmasterHost = "localhost";
    buildmasterPort = workerPort;
    # We need git to be able to get the source code to build it!
    extraPackages = [ pkgs.git ];
  };

  # TODO: Only bind web service to loopback device.
  services.buildbot = {
    enable = true;
    configFile = pkgs.writeText "master.cfg" (import ./templates/master.cfg.nix
    { inherit projectName projectURL workerPort buildbotURL buildbotWebPort;
      inherit gitRepo gitBranch builderName pollInterval workerName workerPassword;
    });
    # We need git to be able to poll for changes.
    extraPackages = [ pkgs.git ];
  };

  services.oauth2_proxy = {
    enable = true;
    provider = "google";
    # XXX: PUPPY: Secrets! How do they work?
    clientID = "579594549675-nuh9d5m5kvdckfdec785o9cbcqe6sje7.apps.googleusercontent.com";
    clientSecret = "bLlMleNsy_63ivGSIktA1EpP";
    cookie = {
      refresh = "1h";
      # XXX: jml doesn't actually understand what this is for.
      # Randomly generated w/ Lastpass.
      secret = "dC1AKxDIlcDzmJOIUh0P1HG7CfoMke4u";
      secure = false;
    };
    httpAddress = "http://0.0.0.0:4180";
    # oauth2_proxy *almost* guesses right. It insists on 'https' though, which
    # isn't being served (yet!) or configured as a valid redirect URL at
    # Google's side.
    redirectURL = "http://buildbot.mumak.net:4180/oauth2/callback";
    upstream = "http://localhost:3000";
    email.addresses = ''
      jonathan.lange@gmail.com
      tehunger@gmail.com
    '';
  };

  services.sshd.enable = true;
  # XXX: '4180' duplicated from httpAddress
  # XXX: After we bind buildbot web service to loopback, remove buildbotWebPort from this list.
  networking.firewall.allowedTCPPorts = [ 22 80 443 4180 ];
}
