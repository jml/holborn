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

      environment = {
          REPO_ROOT = "/tmp";
          PORT = "8080";
      };

      serviceConfig.DevicePolicy = "closed";
      serviceConfig.ExecStart = "${cfg.package}/bin/holborn-repo";
      serviceConfig.User = "holborn-repo";
    };
  };
}
