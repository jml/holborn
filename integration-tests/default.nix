# Run like
# NIX_PATH=. nix-build  default.nix -A
with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
{
  openssh = callPackage ./holborn-openssh-test.nix {};
  http = callPackage ./holborn-protocol-test-1.nix {};
}
