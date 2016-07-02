{ stdenv, git, pkgs }:
rec {
  # Create a single git repository.
  test-repo = stdenv.mkDerivation {
    name = "test-repo";
    srcs = ./.;
    phases = "installPhase";
    installPhase = ''
      set -e
      export PATH=$PATH:${git}/bin
      mkdir -p $out
      cd $out
      export GIT_AUTHOR_NAME="Hieronymous Tester"
      export GIT_AUTHOR_EMAIL="dudebro@example.com"
      export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
      export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
      git init
      echo "hello" > hello
      git add hello
      git commit -m"hello"
    '';
  };

  # Create a store of repositories.
  #
  # Expects a list of sets containing 'id' and 'repo'.
  #
  # e.g. repo-store [ { id = 100; repo = test-repo } ]
  repo-store = name: repos: pkgs.linkFarm name (map (repo: { name = toString repo.id; path = repo.repo; }) repos);
}
