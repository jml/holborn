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

      serviceConfig = {
        ExecStart = ''${cfg.package}/bin/holborn-proxy --port=80 --ssl-port=443 --public-host=https://norf.co --upstream-host=127.0.0.1 --upstream-port=8002 --ssl-full-chain=/var/lib/acme/norf.co/fullchain.pem --ssl-key=/var/lib/acme/norf.co/key.pem --dex-host=http://norf.co:5556  --oauth-client-id="JFiW-C-Oz5vaiTc068Qa452cnIsly9MHEEFV0PpfvS8=@norf.co" --oauth-client-secret="-SEiYMyo5cR6Xkar-JUSkNeRAWJ28FyO5anFZ4xdiS1DDS8CSgmAITHxrlJ0g1mgI7zoz3xWe8nwUQHHpBQGpEZyVahTZXsP"'';

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
