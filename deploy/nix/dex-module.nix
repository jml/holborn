{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.dex;
  emailConfigFile = pkgs.writeText "email-config.json" (builtins.toJSON cfg.emailConfig);
  clientConfigFile = pkgs.writeText "client-config.json" (builtins.toJSON cfg.clientConfig);
  connectorConfigFile = pkgs.writeText "connector-config.json" (builtins.toJSON cfg.connectorConfig);

in
{
  options = {
    services.dex = {
      enable = mkEnableOption "dex daemon and worker";

      package = mkOption {
        default = null;
        type = types.package;
        description = ''
          dex go package.
        '';
      };

      overlordDBURL = mkOption {
        default = null;
        type = types.string;
        description = ''
          The connection URL.
        '';
        example = literalExample ''
          postgres://dex:dex_password@localhost/dex_db?sslmode=disable
        '';
      };
      overlordAdminAPISecret = mkOption {
        default = null;
        type = types.string;
        description = ''
          base64-encoded secret required for API operations.
        '';
      };
      overlordKeySecrets = mkOption {
        default = [];
        type = types.listOf types.string;
        description = ''
          List of base64-encoded secret keys. The first will be used for
          encryption, the rest are for decryption. Pre-pend a new key for
          key rotation.

          Generate e.g. with <command>dd if=/dev/random bs=1 count=32 2>/dev/null | base64 | tr -d '\n'</command>
        '';
      };
      logDebug = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Enable debug logging for worker and overlord.
        '';
      };

      emailConfig = mkOption {
        default = {};
        type = types.attrs;
        description = ''
          JSON email configuration.
        '';
        example = {
            type = "smtp";
            host = "smtp.example.org";
            port = 587;
            auth = "plain";
            username = "postmaster@example.org";
            password = "foo";
        };
      };

      clientConfig = mkOption {
        default = {};
        type = types.attrs;
        description = ''
          Client configuration.
        '';
        example = {
        };
      };

      connectorConfig = mkOption {
        default = [{}];
        type = types.listOf types.attrs;
        description = ''
          Connector configuration.
        '';
        example = [{
          type = "local";
          id = "local";
        }];
      };
    };
  };

  config = mkIf cfg.enable {

    # Make pkg dexctl command available on machine.
    environment.systemPackages = [ cfg.package ];

    users.extraUsers = singleton {
      name = "dex";
      description = "dex user";
    };

    users.extraGroups = singleton {
      name = "dex";
    };

    systemd.services.dex-overlord = {
      description = "dex overlord";
      after = [ "network.target" ];
      requires = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        DEX_OVERLORD_ADMIN_API_SECRET = cfg.overlordAdminAPISecret;
        DEX_OVERLORD_DB_URL = cfg.overlordDBURL;
        # "The dex overlord and workers allow multiple key secrets
        # (separated by commas) to be passed".
        DEX_OVERLORD_KEY_SECRETS = concatStringsSep "," cfg.overlordKeySecrets;
        DEX_OVERLORD_LOG_DEBUG = builtins.toString cfg.logDebug;
      };

      serviceConfig = {
        ExecStart = "${cfg.package.bin}/bin/dex-overlord";
        Restart = "on-failure";
        RestartSec = "10s";
        StartLimitInterval = "1min";
      };
    };

    systemd.services.dex-worker = {
      description = "dex worker";
      after = [ "dex-overlord.service" ];
      requires = [ "dex-overlord.service" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        DEX_WORKER_DB_URL = cfg.overlordDBURL;
        DEX_WORKER_KEY_SECRETS = concatStringsSep "," cfg.overlordKeySecrets;
        DEX_WORKER_ENABLE_REGISTRATION = "true";
        DEX_WORKER_LOG_DEBUG = builtins.toString cfg.logDebug;
        DEX_WORKER_EMAIL_CFG = "${emailConfigFile}";
        DEX_WORKER_HTML_ASSETS = "${cfg.package}/share/go/src/github.com/coreos/dex/static/html/";
        DEX_WORKER_CLIENTS = "${clientConfigFile}";
        DEX_WORKER_CONNECTORS = "${connectorConfigFile}";
      };

      serviceConfig = {
        ExecStart = "${cfg.package.bin}/bin/dex-worker";
        ExecStartPost = "${cfg.package.bin}/bin/dexctl --db-url ${cfg.overlordDBURL} set-connector-configs ${connectorConfigFile}";
        Restart = "on-failure";
        RestartSec = "10s";
        StartLimitInterval = "1min";
      };
    };
  };
}
