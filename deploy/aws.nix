let
    normalSSHPort = 3334;
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
            { fromPort = normalSSHPort; toPort = normalSSHPort; sourceIp = "0.0.0.0/0"; }
            { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
            { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
        ];
    };

    web = { resources, pkgs, lib, config, ... }:
    let
        holborn-openssh-source = pkgs.fetchgitPrivate {
          url = "git@bitbucket.org:tehunger/holborn-ssh.git";
          sha256 = "91e998af03249db570d00262aa5b7b39720b2899b1aa3e86e76bfd10d0299a37";
          rev = "HEAD";
        };
        holborn-openssh = pkgs.callPackage "${holborn-openssh-source}/nix" {};
        holborn-api = pkgs.haskellPackages.callPackage ../holborn-api {};
        holborn-repo = pkgs.haskellPackages.callPackage ../holborn-repo {};
    in
        (common-config // {
        deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.instanceType = "t1.micro";
        deployment.ec2.spotInstancePrice = 7;

        deployment.ec2.keyPair = resources.ec2KeyPairs.pair;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.http-ssh ];


        # The following is slightly messy: The AMI ships with SSH
        # running on port 22 but we're moving it to another port so
        # after server creation and deployment we need to comment out
        # the following line because SSH will have moved to port
        # `normalSSHPort`:
        #deployment.targetPort = 22;

        nixpkgs.config.allowUnfree = true;

        services.openssh.ports = [normalSSHPort];

        environment.systemPackages = [ pkgs.git pkgs.vim ];
        require = [
          ./nix/holborn-openssh-module.nix
          ./nix/holborn-api-module.nix
          ./nix/holborn-repo-module.nix
        ];
        services.holborn-openssh.package = holborn-openssh;
        services.holborn-api.package = holborn-api;
        services.holborn-repo.package = holborn-repo;


        services.postgresql.enable = true;
        services.postgresql.package = pkgs.postgresql94;
        services.postgresql.authentication = ''
        host all all 127.0.0.1/32 trust
        '';
    });
}
