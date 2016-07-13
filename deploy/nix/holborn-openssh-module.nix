# Install & run the holborn-openssh server via systemd. Also makes
# sure we have the correct git user as required by openssh.
{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.holborn-openssh;

  ssh-config = pkgs.writeText "ssh-config" ''
    UsePrivilegeSeparation=no
    PasswordAuthentication=no

    AuthorizedKeysCommand=/holborn-authorized-keys --api-url=${cfg.holbornApiEndpoint} --key=%k --type=%t
    AuthorizedKeysCommandUser=holborn

    # PUPPY not sure we can use those keys they should be regenerated.
    HostKey=/run/keys/ssh_host_rsa_key
    HostKey=/run/keys/ssh_host_dsa_key
    Port=22
    PidFile=/tmp/holborn-openssh.pid
  '';
in
{
  options = {
    services.holborn-openssh = rec {
      package = mkOption {
        type = types.package;
        default = null;
        description = "the OpenSSH package";
      };

      holbornApiEndpoint = mkOption {
        type = types.str;
        default = null;
        description = "holborn-api endpoint e.g. http://127.0.0.1:8002/";
      };

      holbornSshPackage = mkOption {
        type = types.package;
        default = null;
        description = "The holborn-ssh package";
      };

      # client user (i.e. git)
      # server user (e.g. holborn)
      # port (e.g. 22)
    };
  };

  config = {
    users.extraUsers.git = {
      description = "Required so we can use git@... (openssh wants a unix account)";
      createHome = false;
      useDefaultShell = true;
      # User account *must be active*. Do not remove the following:
      # (nobody knows what the password is I never saw it before hashing)
      hashedPassword = "$6$XmkehFEu$Kpae6/dNLuZclOnV.AEoL/bS4C23YOoV6cYYWBUyLnMyw26mK2bioFecAq6uhlztho/G4ecznhQu78RW89Jux.";
    };

    users.extraUsers.holborn = {
      description = "User we run the AuthorizedKeysCommand as";
      createHome = false;
      useDefaultShell = true;
    };

    systemd.services."holborn-openssh" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "holborn-api.service" ];
      after = [ "holborn-api.service" ];

      path = [ cfg.holbornSshPackage ];

      preStart = ''
        cp ${cfg.holbornSshPackage}/bin/holborn-authorized-keys /holborn-authorized-keys
        chown root:root /
        chmod 0755 /
        chown root:root /holborn-authorized-keys
        chmod 0755 /holborn-authorized-keys
      '';

      serviceConfig.ExecStart = "${cfg.package}/bin/sshd -D -e -f ${ssh-config}";
    };
  };
}
