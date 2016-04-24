## Overview

Deployment and packaging for our continuous integration.

Uses [buildbot 0.9](http://buildbot.net).


## How to deploy

```
$ nixops create ./deploy/buildbot-ci.nix ./deploy/buildbot-ci-aws.nix -d buildbot-ci
$ nixops deploy -d buildbot-ci
```

## How it works

### Packaging

Since buildbot 0.9 isn't yet packaged in Nix, we have a bunch of custom
packages for its various components: master, worker, www, pkg.

These packages aren't nixpkgs quality, but they do the job.

### Configuration

Buildbot uses Python for configuration, and you need to specify details about
the network deployment (e.g. local IP of master, names of workers) in the
Python configuration files that get deployed to the master and workers.

## Note on terminology

Buildbot
[used to call workers "slaves"](http://buildbot.readthedocs.org/en/latest/manual/worker-transition.html),
and still uses the old term in places. This set of packages and deployments
tries to use the new term wherever possible.
