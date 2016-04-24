# buildbot1 master configuration for holborn-ci
#
# Would be great to split this between stuff that can be configured at the
# module level (especially stuff about workers and how they relate to the
# master, which is deployment-dependent!) from stuff configuring the actual
# builds, sources, etc.
#
# For now, just munge everything together and do our best to annotate.

{ # Every buildbot project should have these
  projectName, projectURL
# jml is pretty sure that every configuration needs this.
, workerPort ? 9989
# These are only relevant if you're using the web plugin, which you are
# probably using.
, buildbotURL, buildbotWebPort
# Assume one git repository, one branchh, one builder.
, gitRepo, gitBranch, builderName, pollInterval ? 600
# Assuming only one worker for now, which kind of sucks.
, workerName, workerPassword
}:

''
from buildbot.plugins import *

BuildmasterConfig = {
    'workers': [worker.Worker("${workerName}", "${workerPassword}")],

    'protocols': {'pb': {'port': ${toString workerPort}}},

    'change_source': [
        changes.GitPoller(
            '${gitRepo}',
            workdir='gitpoller-workdir', branch='${gitBranch}',
            pollinterval=${toString pollInterval}),
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

    'www': dict(port=${toString buildbotWebPort}),

    'db': {
        'db_url' : "sqlite:///state.sqlite",
    }
}
''
