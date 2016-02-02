# Run like
# NIX_PATH=${NIX_PATH}:. nix-build default.nix [-A openssh]
with (import <nixpkgs> {}).pkgs;
{
  openssh = callPackage ./holborn-openssh-test.nix {};
  http = callPackage ./holborn-protocol-test-1.nix {};
}
