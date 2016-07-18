{ pkgs, stdenv, nodejs-4_x, closurecompiler, nix, node_modules, bower_modules,
  haskellPackages, glibcLocales, zopfli, sassc }:
stdenv.mkDerivation {
  name = "holborn-ui";
  phases = "unpackPhase buildPhase installPhase";
  buildInputs = [
    nodejs-4_x nix
    closurecompiler
    haskellPackages.purescript
    glibcLocales
    zopfli
    sassc
  ];
  src = ../holborn-ui;

  buildPhase = ''
    # Don't do symlinks, enjoy your js life:
    cp -r ${node_modules}/node_modules .
    cp -r ${bower_modules}/bower_components .

    # We're inheriting ./output (src above needs to be more clever)
    rm -rf output

    # Russian comments in `Web.Cookies/foreign.js` need utf8:
    export LANG="en_US.UTF-8"

    NODE_ENV=production node_modules/.bin/webpack --config webpack.config.js --bail
    # cat bundle.js > bundle.min.js
    # cat vendor.bundle.js > vendor.bundle.min.js
    closure-compiler --warning_level QUIET bundle.js > bundle.min.js
    closure-compiler --warning_level QUIET vendor.bundle.js > vendor.bundle.min.js

    # TODO ./static already exists in our source but relying on this
    # implicit contract is probably not a good idea.


    mkdir -p ui/static
    export BUNDLE_HASH=$(nix-hash bundle.min.js)
    export VENDOR_HASH=$(nix-hash vendor.bundle.min.js)
    mv bundle.min.js ui/static/bundle.$BUNDLE_HASH.min.js
    mv vendor.bundle.min.js ui/static/vendor.bundle.$VENDOR_HASH.min.js

    # CSS
    sassc scss/holborn-ui.scss > app.css
    export CSS_HASH=$(nix-hash app.css)
    mv app.css ui/static/app.$CSS_HASH.css


    # Note that zopfli keeps the uncompressed file around. We need
    # that for clients that don't send accept-encoding: gzip
    zopfli -i1000 ui/static/*.min.js
    zopfli -i1000 ui/static/*.css

    cat >ui/index.html <<HEREDOC
    <head>
      <title>norf</title>
      <link href="/static/app.$CSS_HASH.css" rel="stylesheet" type="text/css">
    <body>
      <div id="container" class="container">
        <script>window.holbornBaseUrl = "https://norf.co";</script>
        <script src="/static/vendor.bundle.$VENDOR_HASH.min.js"></script>
        <script src="/static/bundle.$BUNDLE_HASH.min.js"></script>
      </div>
    </body>
    HEREDOC
  '';

  installPhase = ''
    mkdir -p $out
    cp -r ui  $out/
  '';
}
