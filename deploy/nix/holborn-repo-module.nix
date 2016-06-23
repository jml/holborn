# Install & run the holborn-repo server via systemd.
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-repo;
in
{
  options = {
    services.holborn-repo = rec {
      package = mkOption {
        type = types.package;
        default = null;
        description = "the package";
      };

      port = mkOption {
        type = types.int;
        default = 8080;
        description = "port to listen on for web service";
      };

      rawPort = mkOption {
        type = types.int;
        default = 8081;
        description = "port to listen on for raw service";
      };
    };
  };

  config = {
    users.extraUsers.holborn-repo = {
      description = "user to run the service";
      createHome = false;
      useDefaultShell = true;
    };

    systemd.services."holborn-repo" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
      path = [ pkgs.git ];

      serviceConfig.DevicePolicy = "closed";
      serviceConfig.ExecStart = ''\
        ${cfg.package}/bin/holborn-repo \
        --repo-root=/tmp \
        --http-port=${toString cfg.port} \
        --git-port=${toString cfg.rawPort} \
        '';
      serviceConfig.User = "holborn-repo";
    };
  };
}
