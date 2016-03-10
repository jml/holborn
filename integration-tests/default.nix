# Run like
# nix-build --no-out-link default.nix [-A openssh]
with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
in
{
  openssh = callPackage ./holborn-openssh-test.nix { haskellPackages = hp; };
  http = callPackage ./holborn-protocol-test-1.nix { haskellPackages = hp; };
}
