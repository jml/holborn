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
# Where the email comes from
, buildbotFromEmail
# Assume one git repository, one branchh, one builder.
, gitRepo, gitBranch, builderName, pollInterval ? 600
# Assuming only one worker for now, which kind of sucks.
, workerName, workerPassword
}:
''
from holborn_bb import make_config

BuildmasterConfig = make_config(
    worker_name="${workerName}",
    worker_password="${workerPassword}",
    worker_port=${toString workerPort},
    git_repo="${gitRepo}",
    branch="${gitBranch}",
    poll_interval=${toString pollInterval},
    builder_name="${builderName}",
    project_name="${projectName}",
    project_url="${projectURL}",
    buildbot_url="${buildbotURL}",
    buildbot_web_port=${toString buildbotWebPort},
    buildbot_from_email="${buildbotFromEmail}",
)
''
