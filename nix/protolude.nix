{ mkDerivation, async, base, bytestring, containers, deepseq
, ghc-prim, mtl, safe, stdenv, stm, text, transformers
}:
mkDerivation {
  pname = "protolude";
  version = "0.1.6";
  sha256 = "1g9wffqd5la06i2vnvbk6nq9ms08q2cb3i7c7bsp3af13r8z7pfs";
  libraryHaskellDepends = [
    async base bytestring containers deepseq ghc-prim mtl safe stm text
    transformers
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A sensible set of defaults for writing custom Preludes";
  license = stdenv.lib.licenses.mit;
}
