# To run openssh do this:
# nix-shell -p "callPackage ./nix/holborn-ssh.nix {}"
#
{ fetchgit, callPackage }:
let
holborn-openssh-source = fetchgit {
  url = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn-ssh.git";
  sha256 = "181817nwhal4brmpr8881nqmbi5pmb05r99aqpdy00jp31sq46ba";

  # Using HEAD intentionally - if we update the repo this will fail
  # with a hash-mismatch but we probably forgot to pull this along.
  rev = "HEAD";
};
in
callPackage "${holborn-openssh-source}/nix" {}
