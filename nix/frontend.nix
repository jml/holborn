{ pkgs, stdenv, nodejs-4_x, closurecompiler, nix, node_modules, bower_modules, haskellPackages, glibcLocales }:
stdenv.mkDerivation {
  name = "holborn-frontend";
  phases = "unpackPhase buildPhase installPhase";
  buildInputs = [ nodejs-4_x nix closurecompiler haskellPackages.purescript glibcLocales ];
  src = ../holborn-ui;

  buildPhase = ''
    # Don't do symlinks, enjoy your js life:
    cp -r ${node_modules}/node_modules .
    cp -r ${bower_modules}/bower_components .

    # Russian comments in `Web.Cookies/foreign.js` need utf8:
    export LANG="en_US.UTF-8"

    NODE_ENV=production node_modules/.bin/webpack --config webpack.config.js --bail
    cat bundle.js > bundle.min.js
    cat vendor.bundle.js > vendor.bundle.min.js
    # closure-compiler -O WHITESPACE_ONLY bundle.js > bundle.min.js
    # closure-compiler -O WHITESPACE_ONLY vendor.bundle.js > vendor.bundle.min.js

    mkdir static
    export BUNDLE_HASH=$(nix-hash bundle.min.js)
    export VENDOR_HASH=$(nix-hash vendor.bundle.min.js)
    mv bundle.min.js static/bundle.$BUNDLE_HASH.min.js
    mv vendor.bundle.min.js static/vendor.bundle.$VENDOR_HASH.min.js

    cat >index.html <<HEREDOC
    <head>
      <title>shortlog</title>
      <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css" rel="stylesheet" type="text/css">
    <body>
      <div id="container" class="container">
        <script src="/static/vendor.bundle.$VENDOR_HASH.min.js"></script>
        <script src="/static/bundle.$BUNDLE_HASH.min.js"></script>
      </div>
    </body>
    HEREDOC
  '';

  installPhase = ''
    mkdir -p $out
    cp -r index.html static  $out/
  '';
}
