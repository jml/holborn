# Run holborn-proxy (instead of nginx)
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-proxy;
in
{
  options = {
    services.holborn-proxy = rec {
      package = mkOption {
        type = types.package;
        default = null;
        description = "the package";
      };
      httpPort = mkOption { type = types.int; default = 80; description = "http pport"; };
      httpsPort = mkOption { type = types.int; default = 443; description = "https pport"; };
      publicUrl = mkOption { type = types.str; default = "https://norf.co"; description = "publicly visible name"; };
      upstreamHost = mkOption { type = types.str; default = "127.0.0.1"; description = "upstream host"; };
      upstreamPort = mkOption { type = types.int; default = 8002; description = "upstream port"; };
      oauthClientSecret = mkOption {
        type = types.str;
        default = "-SEiYMyo5cR6Xkar-JUSkNeRAWJ28FyO5anFZ4xdiS1DDS8CSgmAITHxrlJ0g1mgI7zoz3xWe8nwUQHHpBQGpEZyVahTZXsP";
        description = "oauth client secret";
      };
      oauthClientId = mkOption {
        type = types.str;
        default = "JFiW-C-Oz5vaiTc068Qa452cnIsly9MHEEFV0PpfvS8=@norf.co";
        description = "oauth client id";
      };
      dexHost = mkOption { type = types.str; default = "http://norf.co:5556"; description = "dex host"; };
    };
  };

  config = {
    users.extraUsers.holborn-proxy = {
      description = "user to run the service";
      createHome = false;
      useDefaultShell = true;
    };

    systemd.services."holborn-proxy" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
      path = [ pkgs.openssh ];

      serviceConfig = {
        ExecStart = ''${cfg.package}/bin/holborn-proxy --port=${toString cfg.httpPort} --ssl-port=${toString cfg.httpsPort} --public-host="${cfg.publicUrl}" --upstream-host="${cfg.upstreamHost}" --upstream-port=${toString cfg.upstreamPort} --ssl-full-chain=/var/lib/acme/norf.co/fullchain.pem --ssl-key=/var/lib/acme/norf.co/key.pem --dex-host=${cfg.dexHost}  --oauth-client-id="${cfg.oauthClientId}" --oauth-client-secret="${cfg.oauthClientSecret}"'';

        PrivateTmp = true;
        PrivateDevices = true;
        NoNewPrivileges = true;

        # Give server minimum set of capabilities needed to run (bind to ports < 1024)
        # and disallow escalating caps thereafter.
        AmbientCapabilities = "CAP_NET_BIND_SERVICE";
        CapabilityBoundingSet = "CAP_NET_BIND_SERVICE";
        PermissionsStartOnly = true;
        # holborn-proxy the only user with access to the certs:
        ExecStartPre = "${pkgs.coreutils}/bin/chown holborn-proxy:root /var/lib/acme/ -R";
        User = "holborn-proxy";
      };
    };
  };
}
