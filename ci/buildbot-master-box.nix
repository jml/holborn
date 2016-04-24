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
  buildbotURL = "http://52.49.196.110:3000/";
  pollInterval = 60; # poll git repo every N seconds

  workerName = "single-host";
  workerPassword = "whatever";

in
{
  require = [
    ./buildbot-module.nix
    ./buildbot-slave-module.nix
  ];

  environment.systemPackages = [];

  services.buildbot-worker = {
    enable = true;
    name = workerName;
    password  = workerPassword;
    adminContact = "Jonathan Lange <jml@mumak.net>";
    hostInfo = "Some EC2 instance somewhere, I guess.";
    buildmasterHost = "localhost";
    buildmasterPort = workerPort;
    # We need git to be able to get the source code to build it!
    extraPackages = [ pkgs.git ];
  };

  services.buildbot = {
    enable = true;
    configFile = pkgs.writeText "master.cfg" (import ./master.cfg.nix
    { inherit projectName projectURL workerPort buildbotURL buildbotWebPort;
      inherit gitRepo gitBranch builderName pollInterval workerName workerPassword;
    });
    # We need git to be able to poll for changes.
    extraPackages = [ pkgs.git ];
  };

  services.sshd.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 80 443 buildbotWebPort ];
}
