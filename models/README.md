# Models

We're using Django to describe our postgres database. This gives us
consistent schemas, migrations and the admin.


# How to add a new model:

First, enter into a shell with Django in it:

```
nix-shell -p python3Packages.django python3Packages.psycopg2
```

Then add new models in e.g. `core/models.py`, followed by

```
./manage.py makemigrations
```

If you have `created_at` timestamps it's a good idea to add default
"now" values to them by adding operations like the following to the migration file (see core/migrations/000x.py):

```
migrations.RunSQL("alter table core_sshkey alter created_at  set default (now() at time zone 'utc')"),
```


At the end apply the changes via

```
./manage.py migrate
```

For all other use cases read the excellent [Django docs](https://docs.djangoproject.com/en/1.10/).

# Important final step:

The integration tests use holborn-api/sql/initial.sql to seed the test
DB so we need to store the schema there as well.

```
pg_dump holborn --schema-only > ../holborn-api/sql/initial.sql
```


# On the server:

We have a command `holborn-manage` in the general environment. You run
e.g. `holborn-manage migrate` to run all migrations.
