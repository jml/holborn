{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-web;
in
{
  options = {
    services.holborn-web = rec {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Run service.";
      };
      package = mkOption {
        type = types.package;
        default = null;
        description = "the package";
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers.holborn-web = {
      description = "user to run the service";
      createHome = false;
      useDefaultShell = true;
    };

    systemd.services."holborn-web" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];

      serviceConfig.ExecStart = "${cfg.package}/bin/holborn";
      serviceConfig.User = "holborn-web";
    };
  };
}
