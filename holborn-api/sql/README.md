# Our data model

To blast the DB away and re-init:

```
psql holborn -f sql/initial.sql
```

To draw a graphviz (dot) diagram run:

```
python sql/render_deps.py
```
