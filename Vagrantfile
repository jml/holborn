# -*- mode: ruby -*-
# vi: set ft=ruby :

# Depends on https://github.com/zimbatm/vagrant-nixos-plugin
#
# Install via:
#   vagrant plugin install vagrant-nixos-plugin

Vagrant.configure(2) do |config|
  # NixOS 15.09
  config.vm.box = "zimbatm/nixos-15.09-x86_64"

  # holborn-ui
  config.vm.network "forwarded_port",
                    guest: 1337,
                    host: 1337,
                    protocol: "tcp"

  config.vm.network "forwarded_port",
                    guest: 8002,
                    host: 8002,
                    protocol: "tcp"

  config.vm.provision :nixos,
    run: 'always',
    path: "vagrant.nix"

end
