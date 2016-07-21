"""Configuration for Holborn's Buildbot."""

from buildbot.plugins import *


def make_config(worker_name, worker_password, worker_port, git_repo, branch,
                poll_interval, builder_name, project_name, project_url,
                buildbot_url, buildbot_web_port, buildbot_from_email):

    return {
        'workers': [worker.Worker(worker_name, worker_password)],
        'protocols': {'pb': {'port': worker_port}},
        'change_source': [
            changes.GitPoller(
                git_repo,
                workdir='gitpoller-workdir',
                branch=branch,
                pollinterval=poll_interval,
            ),
        ],

        'schedulers': [
            schedulers.SingleBranchScheduler(
                name="all",
                change_filter=util.ChangeFilter(branch='branch'),
                treeStableTimer=poll_interval,
                builderNames=[builder_name],
            ),

            schedulers.ForceScheduler(
                name="force",
                builderNames=[builder_name],
            ),
        ],

        'builders': [
            util.BuilderConfig(
                name=builder_name,
                workernames=[worker_name],
                factory=util.BuildFactory([
                    # check out the source
                    steps.Git(repourl=git_repo, mode='incremental'),
                    # run the tests
                    steps.ShellCommand(
                        command=[
                            "direnv", "allow", ".",
                        ],
                    ),
                    steps.ShellCommand(
                        command=[
                            "direnv", "exec", ".", "make", "check",
                        ],
                        env={
                            'NIX_REMOTE': 'daemon',
                        },
                    ),
                ]),
            ),
        ],

        'status': [],

        'title': project_name,
        'titleURL': project_url,

        'buildbotURL': buildbot_url,

        'www': {
            'port': buildbot_web_port,
            'plugins': {
                'waterfall_view': {},
            },
        },

        'db': {
            'db_url': "sqlite:///state.sqlite",
        },

        'services': [
            reporters.MailNotifier(
                fromaddr=buildbot_from_email,
                # TODO(jml): Currently sending mail for all builds. We should
                # send mail under fewer circumstances once we have a better
                # idea about what we actually want.
                #
                # http://buildbot.readthedocs.io/en/latest/manual/cfg-reporters.html?highlight=github#mailnotifier-arguments
                mode='all',
                # XXX: Temporarily hard-code until we can figure out how to
                # get these automatically from commits.
                extraRecipients=[
                    "jml@mumak.net",
                    "thomas.e.hunger@gmail.com",
                ],
            )
        ],
    }
