# Run like
# nix-build --no-out-link default.nix [-A openssh]
with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
in
{
  # TODO openssh integration test is broken until we can populate a
  # test database (maybe a hcl-command?)
  openssh = callPackage ./holborn-openssh-test.nix { haskellPackages = hp; };
  http = callPackage ./holborn-protocol-test-1.nix { haskellPackages = hp; };
}
