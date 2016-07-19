{ haskell, haskellPackages, stdenv, callPackage, fetchgitPrivate, git, writeText
, helpers }:
let
  holborn-repo = haskellPackages.holborn-repo;
  hcl = haskellPackages.hcl;

  repoPort = 8080;
  rawPort = 8081;

  repoId = 100;
  test-repos = helpers.repo-store "test-repos" [ { id = repoId; repo = helpers.test-repo; } ];
in
stdenv.mkDerivation {
  name = "integration-protocol-test-1";
  buildDepends = [ holborn-repo git ];
  srcs = ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
      set -e
      echo "*** holborn-protocol-test-1"

      # Run server (holborn-repo expects git to be available on the PATH)
      PATH=$PATH:${git}/bin ${holborn-repo}/bin/holborn-repo \
        --http-port=${toString repoPort} \
        --git-port=${toString rawPort} \
        --repo-root=${test-repos} &

      # Kill server when test is done
      trap 'kill $(jobs -p)' EXIT

      # Wait for HTTP server to become ready
      ${hcl}/bin/hcl-wait-for-port --port ${toString repoPort} --timeout 5
      ${hcl}/bin/hcl-wait-for-port --port ${toString rawPort} --timeout 5

      # Clone the test repository
      mkdir $out
      pushd $out
      GIT_TRACE=2 ${git}/bin/git clone --verbose http://127.0.0.1:${toString repoPort}/v1/repos/${toString repoId}
      popd

      # The same content?
      expected=$(${git}/bin/git --git-dir ${helpers.test-repo}/.git rev-parse HEAD)
      observed=$(${git}/bin/git --git-dir $out/${toString repoId}/.git rev-parse HEAD)
      [[ $expected == $observed ]]
  '';
}
