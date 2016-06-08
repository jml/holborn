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
        type = types.int;
        default = null;
        description = "port to run on";
      };
      repoServer = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "location of the repo server";
      };
      repoPort = mkOption {
        type = types.int;
        default = 8080;
        description = "port the repo server is listening on for web service";
      };
      rawRepoPort = mkOption {
        type = types.int;
        default = 8081;
        description = "port the repo server is listening on for raw service";
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
          PORT = "${toString cfg.port}";
          HOLBORN_PG_DATABASE = "holborn";
          HOLBORN_PG_USER = "holborn";
          HOLBORN_BASE_URL = "https://norf.co";
          HOLBORN_REPO_HOSTNAME = "${cfg.repoServer}";
          HOLBORN_REPO_PORT = "${toString cfg.repoPort}";
          HOLBORN_REPO_RAW_HOSTNAME = "${cfg.repoServer}";
          HOLBORN_REPO_RAW_PORT = "${toString cfg.rawRepoPort}";
      };

      serviceConfig.ExecStart = "${cfg.package}/bin/holborn-api-server";
      serviceConfig.User = "holborn-api";
    };
  };
}
