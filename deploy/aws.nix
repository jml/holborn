let
    region = "eu-west-1";
    common-config = {
        nix.gc.automatic = true;
        nix.gc.dates = "10:00";
        nix.gc.options = "--delete-older-than 7d";

        networking.firewall.enable = true;
        networking.firewall.allowedTCPPorts = [ 22 80 443 ];
        networking.firewall.allowPing = true;
        services.journald.extraConfig = "SystemMaxUse=100M";

        # Set up ssh keys for git-pushing
        users.extraUsers.root.openssh.authorizedKeys.keyFiles = [
            ./secrets/id_rsa_tom.pub
            ./secrets/id_rsa_jml.pub
        ];

    };
in
rec {
    network.enableRollback = true;
    network.description = "holborn-web";

    resources.ec2KeyPairs.pair =
        { inherit region; };
    resources.ec2SecurityGroups.http-ssh = {
        inherit region;
        rules = [
            { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
            { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
            { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
        ];
    };

    web = { resources, pkgs, lib, ... }:
    let
        holborn-web = pkgs.haskellPackages.callPackage ../holborn-web {};
    in
        (common-config // {
        deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.instanceType = "t1.micro";
        deployment.ec2.spotInstancePrice = 7;

        deployment.ec2.keyPair = resources.ec2KeyPairs.pair;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.http-ssh ];

        environment.systemPackages = [ pkgs.git pkgs.vim ];
        require = [
          ./nix/holborn-web-module.nix
        ];

        services.holborn-web.enable = true;
        services.holborn-web.package = holborn-web;

        services.postgresql.enable = true;
        services.postgresql.package = pkgs.postgresql94;
        services.postgresql.authentication = ''
        host all all 127.0.0.1/32 trust
        '';
    });
}
