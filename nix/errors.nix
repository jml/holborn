{ mkDerivation, base, safe, stdenv, transformers
, transformers-compat, unexceptionalio
}:
mkDerivation {
  pname = "errors";
  version = "2.1.0";
  sha256 = "8689fa17307692eed702a87460506e407f746f2ac1fa2183953cc6204bda0658";
  buildDepends = [
    base safe transformers transformers-compat unexceptionalio
  ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
