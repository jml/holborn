{ mkDerivation, aeson, attoparsec, base, bytestring, pipes
, pipes-attoparsec, pipes-bytestring, pipes-parse, stdenv
, transformers
}:
mkDerivation {
  pname = "pipes-aeson";
  version = "0.4.1.5";
  sha256 = "159m2xq1jyhyglmsydwkq8k19f7cpm7f7igsgi1qs4hlv61mjq3l";
  buildDepends = [
    aeson attoparsec base bytestring pipes pipes-attoparsec
    pipes-bytestring pipes-parse transformers
  ];
  homepage = "https://github.com/k0001/pipes-aeson";
  description = "Encode and decode JSON streams using Aeson and Pipes";
  license = stdenv.lib.licenses.bsd3;
}
