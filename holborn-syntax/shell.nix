# Use unstable channel from Wed 19th of August:
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/cdee2b74eeb6a6bf53a8dabe1c8cba109bf2a4e6.tar.gz) {}).pkgs;
(haskellPackages.callPackage ./. {}).env
