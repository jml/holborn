# Setup

An invocation of nix-shell should get you started with the correct `bower_components` and `node_modules`.

We're using webpack-dev-server:


```console
$ webpack-dev-server
http://localhost:1337/webpack-dev-server/
webpack result is served from /
content is served from /home/tom/devel/holborn/holborn-ui
404s will fallback to /index.html
```

Check e.g. 127.0.0.1/#server/keys


# Sass

We're using http://sass-lang.com/ to compile scss files to css files. Invoke like this:

```
sassc scss/holborn-ui.scss static/holborn-ui.css
```

While developing webpack will take care of that.

# Notes

## Making the build smaller

See ./nix/frontend.nix for the exact invocations needed to make the
build smaller.
