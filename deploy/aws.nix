let
    normalSSHPort = 3334;
    region = "eu-west-1";
    common-config = {
        nix.gc.automatic = true;
        nix.gc.dates = "10:00";
        nix.gc.options = "--delete-older-than 7d";

        networking.firewall.enable = false;
        #networking.firewall.allowedTCPPorts = [ 22 80 443 5556 ];
        #networking.firewall.allowPing = true;
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
            { fromPort = 5556; toPort = 5556; sourceIp = "0.0.0.0/0"; }
            { fromPort = 4180; toPort = 4180; sourceIp = "0.0.0.0/0"; }
        ];
    };

    web = { resources, pkgs, lib, config, ... }:
    let
        hp = pkgs.callPackage ../nix/all-packages.nix {};
        ports = import ./ports.nix;
        holborn-api = hp.callPackage ../holborn-api {};
        holborn-repo = hp.callPackage ../holborn-repo {};
        holborn-ssh = hp.callPackage ../holborn-ssh {};

        # TODO - the following three should live holborn-ui:
        node_modules = pkgs.callPackage ../nix/node_modules.nix {};
        bower_modules = pkgs.callPackage ../nix/bower_modules.nix { inherit node_modules; };
        frontend = pkgs.callPackage ../nix/frontend.nix {
          inherit node_modules bower_modules;
          haskellPackages = hp;
        };
    in
        (common-config // {
        deployment.targetEnv = "ec2";
        deployment.ec2.region = region;
        deployment.ec2.instanceType = "t2.nano"; # $5 / month

        deployment.ec2.keyPair = resources.ec2KeyPairs.pair;
        deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.http-ssh ];


        # The following is slightly messy: The AMI ships with SSH
        # running on port 22 but we're moving it to another port so
        # after server creation and deployment we need to comment out
        # the following line because SSH will have moved to port
        # `normalSSHPort`:
        # deployment.targetPort = 22;

        # Our holborn code is unfree:
        nixpkgs.config.allowUnfree = true;

        require = [
          ./nix/holborn-openssh-module.nix
          ./nix/holborn-api-module.nix
          ./nix/holborn-repo-module.nix
          ./nix/dex-module.nix
          ./nix/oauth2_proxy-module.nix
        ];

        services.openssh.ports = [ normalSSHPort ];

        services.holborn-openssh = {
          package = pkgs.openssh;
          holbornSshPackage = holborn-ssh;
          holbornApiEndpoint = "http://127.0.01:${toString ports.API}";
        };

        services.oauth2_proxy = {
          package = (pkgs.callPackage ../nix/oauth2_proxy.nix {}).bin;
          enable = true;
          provider = "google";

          loginURL = "http://52.50.74.68:5556/auth";
          redeemURL = "http://52.50.74.68:5556/token";
          validateURL = "http://52.50.74.68:5556/keys";
          profileURL = "http://52.50.74.68:5556/";
          redirectURL = "http://52.50.74.68:4180/oauth2/callback";

          cookie.secret = "gei5jo1A fa9ooSh4";
          cookie.secure = false;
          upstream = "http://127.0.0.1:8005/";
          httpAddress = "0.0.0.0:4180";
          clientID = "ULGNrOr4hC9_HakhTBJJzncNUsuef-Y54FjzBj-xPLs=@52.50.74.68";
          clientSecret = "EIZRrGT_925P2OlgM571PQoZnhNv59XXRVLXUmU6WtKX29eT8VOntuqzOtOq3jW_eZarmfcthF54ErRH6WNWrUHZRlmQvm1M";
          scope = "'openid email'";
          email.domains = ["*"];
        };

        services.holborn-api = {
          package = holborn-api;
          port = ports.API;
          repoServer = "127.0.0.1";
          repoPort = ports.REPO;
          rawRepoPort = ports.RAW;
        };

        services.holborn-repo = {
          package = holborn-repo;
          port = ports.REPO;
          rawPort = ports.RAW;
        };

        services.dex = {
          enable = true;
          package = (pkgs.callPackage ../nix/dex.nix { goPackages = pkgs.go16Packages; });
          overlordDBURL = "postgres://dex-rw@127.0.0.1/dex?sslmode=disable";
          overlordAdminAPISecret = "lyErb98S0KpcUJ9AYM3jWlkPLxhvD5czolgJqpjls3jdBslhYqmZUOYhPoi8leiSoLvB6fVZ3xA9KdhZC7UA0COdbhgGORyGLlIq2DY/2xxkPm8UItARTqjSbAfVpqSVzSd1ZEhoNUi+iWNTdTVVArZN2Dg20geMNZEzlx/KqyM=";
          overlordKeySecrets = ["RKLSntSuSsg6Ki8AKmo3WaAw1m3KqTxC3bnGF5i9jCk="];
          logDebug = true;
          emailConfig = {
            type = "fake";
          };
          connectorConfig = [{
            type = "local";
            id = "local";
          }];
        };

        environment.systemPackages = [ pkgs.git pkgs.vim frontend ];

        services.postfix.enable = true;

        services.postgresql.enable = true;
        services.postgresql.package = pkgs.postgresql95;
        services.postgresql.authentication = ''
          host all all 127.0.0.1/32 trust
        '';

        security.acme.certs."norf.co" = {
          webroot = "/var/www/challenges";
          email = "tehunger@gmail.com";
        };

        # For SSL:
        services.nginx.enable = true;
        services.nginx.config = import ./nix/nginx.conf.nix {
          inherit frontend;
          proxy_port = ports.API;
        };
    });
}
