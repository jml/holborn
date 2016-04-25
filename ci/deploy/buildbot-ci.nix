# Logical deployment for Holborn CI.
#
# Basically, everything goes on a single box.

{
  network.description = "Holborn CI";

  buildbot = import ./all-in-one-box.nix;
}
