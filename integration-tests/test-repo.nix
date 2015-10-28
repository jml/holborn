{ stdenv, git }:
stdenv.mkDerivation {
  name = "test-repo";
  srcs = ./holborn-protocol-test-1.nix;
  phases = "installPhase";
  installPhase = ''
      export PATH=$PATH:${git}/bin
      mkdir -p $out/org/hello
      cd $out/org/hello
      git init
      echo "hello" > hello
      git add hello
      git commit -m"hello"
  '';
}
