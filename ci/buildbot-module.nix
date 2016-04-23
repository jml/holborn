# NixOS module for buildbot.
#
# Just runs everything on one machine.

# XXX: some of the twisted stuff modelled on 'kippo'. Maybe we can generalize?

{ config, lib, pkgs, ... }:

with lib;

let
  buildbotDirectory = "/var/run/buildbot";

  # These variables are defined in the configFile but really should be defined up here.
  # - buildbotURL
  # - buildbotWebPort

  # XXX: Depend on our custom packages for now.
  buildbotPackage = pkgs.callPackage ./buildbot-0.9.nix { enableWWW = true; };
  buildbotWebPackage = pkgs.callPackage ./buildbot-www-0.9.nix {};

  cfg = config.services.buildbot;

  buildbotTac =
    pkgs.writeText "buildbot.tac"
    ''
      import os

      from twisted.application import service
      from buildbot.master import BuildMaster

      basedir = '${cfg.runDirectory}'
      configfile = '${cfg.configFile}'
      umask = None

      # note: this line is matched against to check that this is a buildmaster
      # directory; do not edit it.
      application = service.Application('buildmaster')

      # Log to syslog rather than a file somewhere.
      from twisted.python import syslog
      from twisted.python.log import ILogObserver
      application.setComponent(ILogObserver, syslog.SyslogObserver('buildbot').emit)

      m = BuildMaster(basedir, configfile, umask)
      m.setServiceParent(application)
  '';

in
{
  options = {
    services.buildbot = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run buildbot.
        '';
      };

      runDirectory = mkOption {
        type = types.path;
        default = buildbotDirectory;
        description = ''
          Directory where buildbot is run, and its data stored.
        '';
      };

      configFile = mkOption {
        type = types.path;
        description = ''
          Python file that defines a BuildmasterConfig dictionary.
        '';
      };

      pidDirectory = mkOption {
        type = types.path;
        default = cfg.runDirectory;
        description = ''
          Directory to store the pid file in.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    users.extraUsers.buildbot = {
      description = "Buildbot";
      createHome = true;
      home = cfg.runDirectory;
      useDefaultShell = true;
    };

    # XXX: Should be pkgs.buildbot
    environment.systemPackages = [ ];

    systemd.services.buildbot = {
      description = "buildbot master";
      path = [ buildbotPackage buildbotWebPackage ];
      wantedBy = [ "multi-user.target" ];  # XXX: What is this for?
      after = [ "network-interfaces.target" ];

      preStart = ''
        mkdir -m 0755 -p ${cfg.runDirectory}
        # Buildbot expects this to be literally "buildbot.tac"
        cp ${buildbotTac} ${cfg.runDirectory}/buildbot.tac
        # Always try to upgrade the database
        ${buildbotPackage}/bin/buildbot upgrade-master ${cfg.runDirectory}
      '';

      serviceConfig = {
        User = "buildbot";
        Restart = "always";
        RestartSec = 2;
        PermissionsStartOnly = true;  # XXX: What does this mean?

        ExecStart = "${buildbotPackage}/bin/buildbot start --nodaemon ${cfg.runDirectory}";
      };
    };

  };
}
