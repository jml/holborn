let
  region = "eu-west-1";
  # TODO: fix this
  accessKeyId = "holborn-ci"; # symbolic name looked up in ~/.ec2-keys

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      # TODO: Pick a better instance type
      deployment.ec2.instanceType = "t2.micro";
      # TODO: Not sure this is right.
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      deployment.ec2.securityGroups = [
        resources.ec2SecurityGroups.buildbot-security
      ];

      deployment.route53 = {
        inherit accessKeyId;
        hostName = "buildbot.mumak.net";
        usePublicDNSName = true;
      };
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
      # XXX: We could use this security rule to serve on port 80 publicly
      # while not needed root permissions. To jml, that seems like an
      # effective hack but also something that might complicate the deployment
      # going forward
      { fromPort = 4180; toPort = 4180; sourceIp = "0.0.0.0/0"; }
    ];
  };
}
