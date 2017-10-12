# holborn

WIP Git hosting platform. Now deployed at https://norf.co/

## Hacking

This is a mono-repo. Check `README.md` in subfolders to find out what they're
about.

We assume you're using:

* nix
* direnv

## License and Copyright

Copyright (c) 2015-2017 Thomas E. Hunger and Jonathan M. Lange

Made available under GNU AGPL v3

Unless otherwise stated.

## Review

All changes require code review.

Code review should be focused on making sure that you understand what's going
on, that we're not making irreversible mistakes, and identifying low-hanging
fruit for improvement.

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
