# Run like
# nix-build --no-out-link default.nix [-A openssh]
with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
in
{
  http = callPackage ./holborn-protocol-test-1.nix { haskellPackages = hp; };
  buildEverything = callPackage ./build-everything.nix { haskellPackages = hp; };
  # TODO openssh integration test is broken until we can guarantee a user with
  # shell access when building on NixOS.
  # TODO it's also broken until we have finished moving everything to repoIds.
  # openssh = callPackage ./holborn-openssh-test.nix { haskellPackages = hp; };
}
