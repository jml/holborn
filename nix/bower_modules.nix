{ pkgs, stdenv, git, nodejs-4_x, node_modules }:
stdenv.mkDerivation {
  name = "holborn-ui-bower-from-the-internets";
  src = builtins.filterSource (path: type: baseNameOf path == "bower.json") ../holborn-ui;
  phases = "unpackPhase installPhase";
  buildInputs = [ nodejs-4_x git node_modules ];
  installPhase = ''
    mkdir ./npm-home
    mkdir -p $out
    export HOME=$(readlink -f ./npm-home)  # For .npm/cache. Will be discarded.
    cp ./bower.json $out/
    cd $out
    ln -s ${node_modules}/node_modules .
    node_modules/.bin/bower install
  '';
}
