# -*- mode: ruby -*-
# vi: set ft=ruby :

# Depends on https://github.com/zimbatm/vagrant-nixos-plugin
#
# Install via:
#   vagrant plugin install vagrant-nixos-plugin

Vagrant.configure(2) do |config|
  config.vm.box = "rodamber/nixos-16.03-x86_64"

  config.vm.provider "virtualbox" do |v|
    v.memory = 4096
  end

  config.vm.provision :nixos,
    run: 'always',
    path: "vagrant.nix"

end
