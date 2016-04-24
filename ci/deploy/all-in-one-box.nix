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
  # XXX: Specifying this in the *machine* configuration is wrong. Should be in
  # the network / deployment configuration.
  buildbotURL = "http://52.48.211.75:3000/";
  # TODO: Change this to 5 minutes once we're done configuring the server.
  pollInterval = 60; # poll git repo every N seconds

  workerName = "single-host";
  workerPassword = "whatever";

in
{
  require = [
    ../modules/buildbot-master.nix
    ../modules/buildbot-worker.nix
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

  services.buildbot = {
    enable = true;
    configFile = pkgs.writeText "master.cfg" (import ./templates/master.cfg.nix
    { inherit projectName projectURL workerPort buildbotURL buildbotWebPort;
      inherit gitRepo gitBranch builderName pollInterval workerName workerPassword;
    });
    # We need git to be able to poll for changes.
    extraPackages = [ pkgs.git ];
  };

  services.sshd.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 80 443 buildbotWebPort ];
}
