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

      serviceConfig.ExecStart = "${cfg.package}/bin/holborn-api-server";
      serviceConfig.User = "holborn-api";
    };
  };
}
