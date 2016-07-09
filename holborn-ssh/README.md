# holborn-ssh

Tools for providing an SSH server frontend for Holborn.

## holborn-authorized-keys

Command-line tool to query the `holborn-api` server and get a valid
`authorized_keys` file for a particular user, to be used by OpenSSH.

It's a bit tricky to configure correctly, here are some notes:

* sshd_config must specify `AuthorizedKeysCommandUser` (use a dedicated one)
* login user must have valid shell on the server, even if it's never run
* `AuthorizedKeysCommand` must have absolute path
* `AuthorizedKeysCommand` must be writeable only by `root`, and only in
  directories that are only writeable by `root`
* you must run `sshd` as `root` in order to be able to execute the script as `AuthorizedKeysCommandUser`

When debugging, be sure to watch out for:

* login user (e.g. `git`) must have ssh keys on the client side, otherwise
  `AuthorizedKeysCommand` is not run on the server
* the stdout of `AuthorizedKeysCommand` is logged at Debug2 level on the SSH server
* the stderr doesn't go anywhere obvious
* Running `git` against the server often provides very little information. To
get more, SSH directly, providing a valid git network command, e.g. ```
ssh git@<host> git-receive-pack \'/<org>/<repo>\'
```

## holborn-connect-repo

Command-line tool intended to be supplied as the `ForceCommand` option for a
user after they have authenticated with `holborn-authorized-keys`. The idea is
that `holborn-connect-repo` will be called with the repository the user is
trying to access and will then use this info to consult `holborn-api` to
determine whether the user can access that repository. It will then forward
traffic to the correct `holborn-repo` backend server.
