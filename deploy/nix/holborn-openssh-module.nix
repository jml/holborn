# Install & run the holborn-openssh server via systemd. Also makes
# sure we have the correct git user as required by openssh.
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-openssh;
  ssh-config = pkgs.writeText "ssh-config" ''
    UsePrivilegeSeparation=no
    PasswordAuthentication=no

    # PUPPY not sure we can use those keys they should be regenerated.
    HostKey=${cfg.package}/etc/ssh_host_rsa_key
    HostKey=${cfg.package}/etc/ssh_host_dsa_key
    Port=22
    PidFile=/tmp/holborn-openssh.pid
    HolbornApiEndpoint=http://127.0.0.1:8002
  '';
in
{
  options = {
    services.holborn-openssh = rec {
      package = mkOption {
        type = types.package;
        default = null;
        description = "the package";
      };
    };
  };

  config = {
    users.extraUsers.git = {
      description = "Required so we can use git@... (openssh wants a unix account)";
      createHome = false;
      useDefaultShell = true;
      hashedPassword = "$6$XmkehFEu$Kpae6/dNLuZclOnV.AEoL/bS4C23YOoV6cYYWBUyLnMyw26mK2bioFecAq6uhlztho/G4ecznhQu78RW89Jux.";
    };

    systemd.services."holborn-openssh" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "holborn-api.service" ];
      after = [ "holborn-api.service" ];

      serviceConfig.ExecStart = "${cfg.package}/bin/sshd -D -e -f ${ssh-config}";
    };
  };
}
