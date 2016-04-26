{ stdenv, git }:
stdenv.mkDerivation {
  name = "test-repo";
  srcs = ./holborn-protocol-test-1.nix;
  phases = "installPhase";
  installPhase = ''
      set -ex
      echo "*** test-repo"
      export PATH=$PATH:${git}/bin
      mkdir -p $out/org/hello
      cd $out/org/hello
      export GIT_AUTHOR_NAME="Hieronymous Tester"
      export GIT_AUTHOR_EMAIL="dudebro@example.com"
      export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
      export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
      git init
      echo "hello" > hello
      git add hello
      git commit -m"hello"
  '';
}
