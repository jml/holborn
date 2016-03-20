# holborn

Now deployed at https://norf.co/

## Goal

Provide a code hosting and collaboration platform that gets out of people's
way while they make great software.

## Hacking

This is a mono-repo. Check `README.md` in subfolders to find out what they're
about.

We assume you're using:

* nix
* direnv

## Review

All changes require code review. We're currently in "claiming territory"
phase, which means that the focus is on delivering functionality without
screwing ourselves over wrt scalability or long-term maintenance.

Code review should be focused on making sure that you understand what's going
on, that we're not making irreversible mistakes, and identifying low-hanging
fruit for improvement.

## Documentation

There are various docs on
[Google Drive](https://drive.google.com/drive/folders/0BzzRizsvL_4ONVBMZU9wSlkwOTg).

Please update [reading-material.md](reading-material.md) with interesting
things you find about Git, DVCS, or development collaboration.

## Running locally

To run locally, you need several components:

* holborn-repo
* a postgresql database
* holborn-api
* holborn-ui

### holborn-repo

```
cd holborn-repo
REPO_ROOT=$HOME/src PORT=8080 nix-shell --command "cabal run"
```

### holborn-api

First set up the database as documented in holborn-api/README.md.

```
cd holborn-api
nix-shell --command "cabal run"
```

### holborn-ui

```
cd holborn-ui
nix-shell --command "pulp server"
```

Once all these steps are done, go to http://localhost:8002 in the web browser.
You should see a login screen.
