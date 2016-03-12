{ callPackage, nodejs, stdenv, haskellPackages, sassc }:
let
  node_modules = callPackage ../nix/node_modules.nix {};
  bower_modules = callPackage ../nix/bower_modules.nix { inherit node_modules; };
in
stdenv.mkDerivation {
  name = "holborn-ui";
  src = ./.;
  buildInputs = [
    nodejs
    haskellPackages.purescript
    haskellPackages.psc-ide
    sassc
    node_modules
    bower_modules
    sassc
  ];
  shellHook = ''
    rm -rf node_modules bower_components

    # Can't symlink because js developers have never heard of such a
    # thing.
    cp -r ${node_modules}/node_modules .
    cp -r ${bower_modules}/bower_components .
    chmod u+w node_modules bower_components -R
  '';
}
