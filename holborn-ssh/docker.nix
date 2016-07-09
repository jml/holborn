# Docker image for holborn-ssh
#
# Build with:
#   nix-build ./docker.nix
#
# Load with:
#   docker load < result
#
# Run with
#   docker run holborn-ssh:latest

{ lib ? (import <nixpkgs> {}).lib, pkgs ? import <nixpkgs> {} }:

with lib;
with pkgs;

let
  ssh-config = { openssh, port, holborn-ssh, api-url, username }: writeText "ssh-config" ''
    UsePrivilegeSeparation=no
    PasswordAuthentication=no

    ${flip concatMapStrings hostKeys (k: ''
      HostKey ${k.path}
    '')}
    Port=${toString port}
    PidFile=/openssh.pid
    AuthorizedKeysCommand=/holborn-authorized-keys --api-url=${api-url} --key=%k --type=%t
    AuthorizedKeysCommandUser=${username}
  '';

  sshImage = { clientUser, serverUser, port, openssh, api-url, holborn-ssh }: dockerTools.buildImage {
    name = "holborn-ssh";
    tag = "latest";

    contents = [ bash holborn-ssh ];

    # PUPPY: We use an insecure password for the user we create to run
    # AuthorizedKeysCommand. We have to set a password because SSH won't allow
    # "inactive" users to log in.
    runAsRoot = ''
      #!${stdenv.shell}
      ${dockerTools.shadowSetup}

      groupadd -r ${serverUser}
      useradd -r -g ${serverUser} -M ${serverUser}

      groupadd -r ${clientUser}
      useradd -r -g ${clientUser} -M ${clientUser} -s ${bash}/bin/bash
      echo ${clientUser}:insecure-password | chpasswd

      cp ${holborn-ssh}/bin/holborn-authorized-keys /holborn-authorized-keys
      chown root:root /
      chmod 755 /
      chown root:root /holborn-authorized-keys
      chmod 0755 /holborn-authorized-keys

      ${flip concatMapStrings hostKeys (k: ''
        if ! [ -f "${k.path}" ]; then
            mkdir -p $(dirname "${k.path}")
            ${openssh}/bin/ssh-keygen -t "${k.type}" ${if k ? bits then "-b ${toString k.bits}" else ""} -f "${k.path}" -N ""
        fi
      '')}
    '';

    config = {
      EntryPoint = [ "${openssh}/bin/sshd" "-Def" "${ssh-config { openssh = openssh; port = port; api-url = api-url; username = serverUser; holborn-ssh = holborn-ssh; } }" ];
      ExposedPorts = {
        "${toString port}/tcp" = {};
      };
    };
  };

  holborn-ssh = (callPackage ../nix/all-packages.nix {}).callPackage ../holborn-ssh {};

  hostKeys = [ { type = "rsa"; bits = 4096; path = "/etc/ssh/ssh_host_rsa_key"; } ];
in
{
  sshImage = sshImage;
  # TODO: Might be nice to API server & port as command-line parameters to the
  # docker image.
  # TODO: Has to be IP address of docker bridge
  holborn-ssh-docker = sshImage {
    port = 22;
    clientUser = "git";
    serverUser = "holborn";
    openssh = openssh;
    holborn-ssh = holborn-ssh;
    api-url = "http://172.17.0.1:8002/";
  };
}
