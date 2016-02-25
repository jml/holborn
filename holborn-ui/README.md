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


# Making the build smaller

The debug build is 1.4M. For production we want a smaller build which
we can do like so:

```
rm -rf output # Not sure why needed but pulp breaks without ("Prelude not found")
NODE_ENV=production pulp browserify -O -t app.js
closure-compiler app.js -O SIMPLE > app.min.js
zopfli app.min.js
```

Eventually this will go into a nix build of course.
