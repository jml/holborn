Needs node-4, which is not in NixOS 15.09

```
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz nix-shell
```

```
$ npm install
$ bower install
$ pulp server  # --host 0.0.0.0 if running from inside Vagrant
* Server listening on http://localhost:1337/
```

Check e.g. 127.0.0.1/#server/keys


# Sass

We're using http://sass-lang.com/ to compile scss files to css files. Invoke like this:

```
sassc scss/holborn-ui.scss static/holborn-ui.css
```

If developing a lot it'll be easier to run with a filesystem watcher, e.g.:

```
while inotifywait -e close_write,moved_from -r scss/; do sassc scss/holborn-ui.scss static/holborn-ui.css; done
```


# Notes

## Making the build smaller

See ./nix/frontend.nix for the exact invocations needed to make the
build smaller.
