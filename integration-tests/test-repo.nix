{ stdenv, git }:
stdenv.mkDerivation {
  name = "test-repo";
  srcs = ./holborn-protocol-test-1.nix;
  phases = "installPhase";
  installPhase = ''
      export PATH=$PATH:${git}/bin
      mkdir $out
      cd $out
      git init
      echo "hello" > hello
      git add hello
      git commit -m"hello"
  '';
}
