{ config, lib, pkgs, ... }:

with lib;

let
  buildbotDirectory = "/var/run/buildbot-worker";

  # XXX: Depend on custom package for now.
  buildbotWorkerPackage = pkgs.callPackage ./buildbot-slave-0.9.nix {};
  buildbotWorkerCommand = "${buildbotWorkerPackage}/bin/buildslave";

  cfg = config.services.buildbot-worker;
  tacFile = pkgs.writeText "buildbot.tac" (
    import ./buildbot-slave.tac.nix {
      host = cfg.buildmasterHost;
      port = cfg.buildmasterPort;
      name = cfg.name;
      password = cfg.password;
      runDirectory = cfg.runDirectory;
      # TODO: Config variables for these.
      keepAlive = 600;
      maxDelay = 300;
    }
  );
  adminFile = pkgs.writeText "admin"
    ''
    ${cfg.adminContact}
    '';
  hostInfoFile = pkgs.writeText "info"
    ''
    ${cfg.hostInfo}
    '';

in
{
  options = {
    services.buildbot-worker = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run a buildbot worker.
        '';
      };

      buildmasterHost = mkOption {
        type = types.str;
        description = ''
          The hostname of the buildbot master.
        '';
        example = "buildmaster.example.com";
      };

      buildmasterPort = mkOption {
        type = types.int;
        default = 9989;
        description = ''
          The port on the buildbot master to connect to.
        '';
        example = 9989;
      };

      name = mkOption {
        type = types.str;
        description = ''
          Name that uniquely identifies this worker.
        '';
        example = "worker-24601";
      };

      password = mkOption {
        type = types.str;
        description = ''
          Secret shared between the master and this worker.
        '';
        example = "secret";
      };

      runDirectory = mkOption {
        type = types.path;
        default = buildbotDirectory;
        description = ''
          Directory where buildbot worker is run, and its data stored.
        '';
      };

      adminContact = mkOption {
        type = types.str;
        description = ''
          Administrator contact details.
        '';
        example = "Your Name Here <admin@example.com>";
      };

      hostInfo = mkOption {
        type = types.str;
        description = ''
          Information about the host the worker is running on.

          Will be displayed on the buildbot master web UI.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    users.extraUsers.buildbot-worker = {
      description = "Buildbot worker";
      createHome = true;
      home = cfg.runDirectory;
      useDefaultShell = true;
    };

    environment.systemPackages = [ buildbotWorkerPackage ];

    systemd.services.buildbot-worker = {
      description = "buildbot worker";
      path = [ buildbotWorkerPackage ];
      wantedBy = [ "multi-user.target" ];  # XXX: What is this for?
      after = [ "network-interfaces.target" ];

      preStart = ''
        mkdir -m 0755 -p ${cfg.runDirectory}/info
        # Buildbot expects this to be literally "buildbot.tac"
        cp ${tacFile} ${cfg.runDirectory}/buildbot.tac
        cp ${hostInfoFile} ${cfg.runDirectory}/info/host
        cp ${adminFile} ${cfg.runDirectory}/info/admin
      '';

      serviceConfig = {
        User = "buildbot-worker";
        Restart = "always";
        RestartSec = 2;
        PermissionsStartOnly = true;  # XXX: What does this mean?

        ExecStart = "${buildbotWorkerCommand} start --nodaemon ${cfg.runDirectory}";
      };
    };

  };
}
