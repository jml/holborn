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

      environment = {
          PORT = "80";
          SSL_PORT = "443";
          HOLBORN_PUBLIC_HOST = "https://norf.co";
          HOLBORN_UPSTREAM_HOST = "127.0.0.1";
          HOLBORN_UPSTREAM_PORT = "8002";
          HOLBORN_DEX_HOST = "http://norf.co:5556";
          HOLBORN_SSL_FULL_CHAIN = "/var/lib/acme/norf.co/fullchain.pem";
          HOLBORN_SSL_KEY = "/var/lib/acme/norf.co/key.pem";

          # The following have been registered on the server running dex-worker like so:
          # dexctl --db-url="postgres://dex-rw@127.0.0.1/dex?sslmode=disable" new-client https://norf.co/oauth2/callback
          HOLBORN_OAUTH_CLIENT_ID = "JFiW-C-Oz5vaiTc068Qa452cnIsly9MHEEFV0PpfvS8=@norf.co";
          HOLBORN_OAUTH_CLIENT_SECRET = "-SEiYMyo5cR6Xkar-JUSkNeRAWJ28FyO5anFZ4xdiS1DDS8CSgmAITHxrlJ0g1mgI7zoz3xWe8nwUQHHpBQGpEZyVahTZXsP";
      };

      serviceConfig = {
        PrivateTmp = true;
        PrivateDevices = true;
        AmbientCapabilities = "CAP_NET_BIND_SERVICE";
        CapabilityBoundingSet = "CAP_NET_BIND_SERVICE";
        PermissionsStartOnly = true;
        # holborn-proxy the only user with access to the certs:
        ExecStartPre = "${pkgs.coreutils}/bin/chown holborn-proxy:root /var/lib/acme/ -R";
        ExecStart = "${cfg.package}/bin/holborn-proxy";
        User = "holborn-proxy";
      };
    };
  };
}
