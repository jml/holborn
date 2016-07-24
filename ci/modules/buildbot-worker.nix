{ config, lib, pkgs, ... }:

with lib;

let
  buildbotDirectory = "/var/run/buildbot-worker";

  # XXX: Depend on custom package for now.
  buildbotWorkerPackage = (import ../packages).buildbot-worker;
  buildbotWorkerCommand = "${buildbotWorkerPackage}/bin/buildbot-worker";
  # This grotesque hack works around what jml thinks is a systemd limitation
  # on the length of environment variables.
  #
  # Really we should only do this wrapping if nix builds are enabled, but eh.
  buildbotWorkerScript = pkgs.writeScript "buildbot-worker" ''
    #!/usr/bin/env bash

    export PATH=${lib.makeBinPath nixPackages}:$PATH
    ${buildbotWorkerCommand} "$@"
  '';

  cfg = config.services.buildbot-worker;
  tacFile = pkgs.writeText "buildbot.tac" (
    import ./templates/buildbot-worker.tac.nix {
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
  nixConfigFile = pkgs.writeText "config.nix" (
    if cfg.allowUnfree
    then "{ allowUnfree = true; }"
    else "");

  # Standard things for building nix.
  # Cribbed from hydra/release.nix.
  nixPackages = with pkgs; [
    nix
    gnutar
    coreutils
    findutils
    gzip
    bzip2
    unzip
    git
    gitAndTools.topGit
    gnused
  ];

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

      extraGroups = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          Groups that the worker needs to be a member of in order to run the
          tests.
        '';
      };

      extraPackages = mkOption {
        type = types.listOf types.package;
        default = [];
        description = ''
          Packages that will be available to the worker service.
        '';
      };

      enableNixBuilds = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether or not the worker is allowed to do nix-builds.

          Enabling this will add nix to the worker path and grant worker user
          permissions to build nix packages.
        '';
      };

      allowUnfree = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Is the worker allowed to do unfree nix-builds?
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    users.extraUsers.buildbot-worker = {
      description = "Buildbot worker";
      createHome = true;
      home = cfg.runDirectory;
      extraGroups = cfg.extraGroups;
      useDefaultShell = true;
    };

    nix.trustedUsers = if cfg.enableNixBuilds then [ "buildbot-worker" ] else [];

    environment.systemPackages = [ ];

    systemd.services.buildbot-worker = {
      description = "buildbot worker";
      path = [ buildbotWorkerPackage ] ++ cfg.extraPackages;
      wantedBy = [ "multi-user.target" ];
      after = [ "network-interfaces.target" ];

      preStart = ''
        mkdir -m 0755 -p ${cfg.runDirectory}/info
        # Buildbot expects this to be literally "buildbot.tac"
        cp ${tacFile} ${cfg.runDirectory}/buildbot.tac
        cp ${hostInfoFile} ${cfg.runDirectory}/info/host
        cp ${adminFile} ${cfg.runDirectory}/info/admin

        # Configure nixpkgs. Only relevant if we're using nix-build on the worker.
        mkdir -m 0755 -p ${cfg.runDirectory}/.nixpkgs
        cp ${nixConfigFile} ${cfg.runDirectory}/.nixpkgs/config.nix
      '';

      serviceConfig = {
        User = "buildbot-worker";
        Restart = "always";
        RestartSec = 2;
        PermissionsStartOnly = true;  # XXX: What does this mean?

        ExecStart = "${buildbotWorkerScript} start --nodaemon ${cfg.runDirectory}";
      };
    };

  };
}
