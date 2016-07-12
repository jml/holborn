# Run like
# nix-build --no-out-link default.nix [-A openssh]
with (import <nixpkgs> {}).pkgs;
let
  hp = callPackage ../nix/all-packages.nix {};
  helpers = callPackage ./helpers.nix {};
in
{
  http = callPackage ./holborn-protocol-test-1.nix { haskellPackages = hp; helpers = helpers; };
  buildEverything = callPackage ./build-everything.nix { haskellPackages = hp; };
  ssh-authz = callPackage ./holborn-ssh-authz-test.nix { haskellPackages = hp; helpers = helpers; };
}
