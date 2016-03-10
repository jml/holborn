# To run openssh do this:
# nix-shell -p "callPackage ./nix/holborn-ssh.nix {}"
#
{ fetchgit, callPackage }:
let
holborn-openssh-source = fetchgit {
  url = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn-ssh.git";
  sha256 = "0n2wvqj311g9hm682ga0khswndj00djhc1xmlq7f4587kvii9vdf";

  # Using HEAD intentionally - if we update the repo this will fail
  # with a hash-mismatch but we probably forgot to pull this along.
  rev = "HEAD";
};
in
callPackage "${holborn-openssh-source}/nix" {}
