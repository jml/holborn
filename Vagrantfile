# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  # NixOS 15.09
  config.vm.box = "zimbatm/nixos-15.09-x86_64"

  # holborn-ui
  config.vm.network "forwarded_port",
                    guest: 1337,
                    host: 1337,
                    protocol: "tcp"

  config.vm.provision :nixos,
    run: 'always',
    path: "vagrant.nix"

end
