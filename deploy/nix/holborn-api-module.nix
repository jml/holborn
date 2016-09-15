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

      serviceConfig.ExecStart = ''${cfg.package}/bin/holborn-api-server --port=${toString cfg.port} --postgres-database="holborn" --postgres-user="holborn" --repo-hostname="${cfg.repoServer}" --repo-http-port=${toString cfg.repoPort}  --repo-git-port=${toString cfg.rawRepoPort}'';
      serviceConfig.User = "holborn-api";
    };
  };
}
