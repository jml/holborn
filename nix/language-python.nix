{ mkDerivation, alex, array, base, containers, happy, monads-tf
, pretty, stdenv, transformers, utf8-string
, fetchFromGitHub
}:
mkDerivation {
  pname = "language-python";
  version = "0.5.3";
  # To build on ghc 8.0, we need to allow transformers 0.5.
  # See https://github.com/bjpop/language-python/pull/29
  src = fetchFromGitHub {
    owner = "jml";
    repo = "language-python";
    rev = "new-transformers";
    sha256 = "03hpaaxg8wz7c5qz28h3rv5fzpmyyxg1c7gxh88qmd0pynk044dn";
  };
  buildDepends = [
    array base containers monads-tf pretty transformers utf8-string
  ];
  buildTools = [ alex happy ];
  homepage = "http://github.com/bjpop/language-python";
  description = "Parsing and pretty printing of Python code";
  license = stdenv.lib.licenses.bsd3;
}
