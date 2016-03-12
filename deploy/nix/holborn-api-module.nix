# Install & run the holborn-api server via systemd.
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-api;
in
{
  options = {
    services.holborn-api = rec {
      package = mkOption {
        type = types.package;
        default = null;
        description = "the package";
      };
      port = mkOption {
        type = types.str;
        default = null;
        description = "port to run on";
      };
    };
  };

  config = {
    users.extraUsers.holborn-api = {
      description = "user to run the service";
      createHome = false;
      useDefaultShell = true;
    };

    systemd.services."holborn-api" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
      path = [ pkgs.openssh ];

      environment = {
          PORT = "${cfg.port}";
          HOLBORN_PG_DATABASE = "holborn";
          HOLBORN_PG_USER = "holborn";
          HOLBORN_BASE_URL = "https://norf.co";
      };

      serviceConfig.ExecStart = "${cfg.package}/bin/holborn-api-server";
      serviceConfig.User = "holborn-api";
    };
  };
}
