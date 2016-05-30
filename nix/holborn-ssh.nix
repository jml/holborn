# To run openssh do this:
# nix-shell -p "callPackage ./nix/holborn-ssh.nix {}"
#
{ fetchgit, callPackage }:
let
holborn-openssh-source = fetchgit {
  url = "https://holbornlondon:DSmiB2AVZJhftk4XRyH1N98XNMYzOmY9@bitbucket.org/holbornlondon/holborn-ssh.git";
  sha256 = "0rgabrh7r67pipca7igczhv4d92wim6pvp9vgv25hdnbb622mw66";

  # Using HEAD intentionally - if we update the repo this will fail
  # with a hash-mismatch but we probably forgot to pull this along.
  rev = "HEAD";
};
in
callPackage "${holborn-openssh-source}/nix" {}
