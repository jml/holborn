# A box running all the components of Buildbot
{ config, pkgs, lib, ... }:
let
  workerPort = "9989";
  gitRepo = "https://bitbucket.com/holbornlondon/holborn";
  gitBranch = "master";
  builderName = "holborn-experimental-builder";
  projectName = "holborn";
  projectURL = "https://bitbucket.com/holbornlondon/holborn";
  buildbotWebPort = "3000";
  buildbotURL = "https://jml.io";
  pollInterval = "600"; # seconds

  workerName = "single-host";
  workerPassword = "whatever";

in
{
  require = [
    ./buildbot-module.nix
    ./buildbot-slave-module.nix
  ];

  services.buildbot-worker = {
    enable = true;
    name = workerName;
    password  = workerPassword;
    adminContact = "Jonathan Lange <jml@mumak.net>";
    hostInfo = "Some EC2 instance somewhere, I guess.";
    buildmasterHost = "localhost";
  };

  services.buildbot = {
    enable = true;
    configFile = pkgs.writeText "master.cfg"
      ''
    from buildbot.plugins import *

    BuildmasterConfig = {
        ####### WORKERS

        # The 'workers' list defines the set of recognized workers. Each element is
        # a Worker object, specifying a unique worker name and password.  The same
        # worker name and password must be configured on the worker.
        'workers': [worker.Worker("${workerName}", "${workerPassword}")],

        'protocols': {'pb': {'port': ${workerPort}}},

        ####### CHANGESOURCES

        # the 'change_source' setting tells the buildmaster how it should find out
        # about source code changes.  Here we point to the buildbot clone of pyflakes.
        'change_source': [
            changes.GitPoller(
                '${gitRepo}',
                workdir='gitpoller-workdir', branch='${gitBranch}',
                pollinterval=${pollInterval}),
        ],

        'schedulers': [
            schedulers.SingleBranchScheduler(
                name="all",
                change_filter=util.ChangeFilter(branch='${gitBranch}'),
                treeStableTimer=None,
                builderNames=["${builderName}"]),
            schedulers.ForceScheduler(
                name="force",
                builderNames=["${builderName}"]),
        ],

        'builders': [
            util.BuilderConfig(
                name="${builderName}",
                workernames=["${workerName}"],
                factory=util.BuildFactory([
                    # check out the source
                    steps.Git(repourl='${gitRepo}', mode='incremental'),
                    # run the tests (note that this will require that 'trial' is installed)
                    steps.ShellCommand(command=["true"]),
                ]),
            ),
        ],

        'status': [],

        'title': '${projectName}',
        'titleURL': "${projectURL}",

        'buildbotURL': "${buildbotURL}",

        'www': dict(port=${buildbotWebPort}),

        'db': {
            'db_url' : "sqlite:///state.sqlite",
        }
    }

      '';
  };

  services.sshd.enable = true;
  # XXX: Duplicates buildbotWebPort
  networking.firewall.allowedTCPPorts = [ 22 80 443 3000 ];
}
