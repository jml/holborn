let
  region = "eu-west-1";
  # TODO: fix this
  accessKeyId = "holborn-ci"; # symbolic name looked up in ~/.ec2-keys

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2 = {
        inherit accessKeyId region;
        ebsInitialRootDiskSize = 100;
        instanceType = "t2.nano";
        keyPair = resources.ec2KeyPairs.my-key-pair;
        securityGroups = [
          resources.ec2SecurityGroups.buildbot-security
        ];
      };

      deployment.route53 = {
        inherit accessKeyId;
        hostName = "buildbot.mumak.net";
        usePublicDNSName = true;
      };

      # Because we're building Haskell packages on a t2.nano instance we need
      # some swap to fit everything in memory.
      swapDevices = [
        { device = "/swap";
          size = 4096;  # Megabytes
        }
      ];
    };

in
{
  buildbot    = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };

  resources.ec2SecurityGroups.buildbot-security = {
    inherit region accessKeyId;
    # XXX: Duplicates information from all-in-one-box port list.
    rules = [
      { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
      { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
      { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
    ];
  };
}
