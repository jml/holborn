# holborn-api server

* Presents a RESTful frontend to our database
* All access to database goes through here
* Responsible for
  * authentication
  * authorization

## Dependencies

Make sure that the following are installed:

* postgresql

## Set up

### Database

Choose a directory for the database. Note that it cannot be in the `/vagrant`
shared directory. e.g.

```
export PGDATA=/home/vagrant/database
```

Then create the database:

```
initdb ${PGDATA}
postgres -D ${PGDATA} &
createuser holborn
createdb holborn
psql holborn -f sql/initial.sql
```

(see also [sql/README.md](sql/README.md))


## Running

Inside a `nix-shell`, do:

```
cabal run
```

Run with `--help` to get detailed options:

The server will listen on port 8002 by default.

Make sure that holborn-ui is also running on the same server on port 1337 (use
`webpack-dev-server`).
