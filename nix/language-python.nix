{ mkDerivation, alex, array, base, containers, happy, monads-tf
, pretty, stdenv, transformers, utf8-string
}:
mkDerivation {
  pname = "language-python";
  version = "0.5.2";
  sha256 = "0dv48i8mlyvk1a32zk3s83fw61rh40cmyx49kcmw6qna17ibv2vc";
  buildDepends = [
    array base containers monads-tf pretty transformers utf8-string
  ];
  buildTools = [ alex happy ];
  homepage = "http://github.com/bjpop/language-python";
  description = "Parsing and pretty printing of Python code";
  license = stdenv.lib.licenses.bsd3;
}
