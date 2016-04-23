# Logical deployment for Holborn CI.
#
# Basically, everything goes on a single box.

{
  network.description = "Holborn CI";

  buildbot = import ./buildbot-master-box.nix;
}
