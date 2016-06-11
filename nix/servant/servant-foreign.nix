{ mkDerivation, base, hspec, http-types, lens, servant, text
, forked-servant
, stdenv
}:
mkDerivation rec {
  pname = "servant-foreign";
  version = "0.7.1";
  src = forked-servant;
  preCompileBuildDriver = ''
    cd ${pname}/
  '';
  libraryHaskellDepends = [ base http-types lens servant text ];
  testHaskellDepends = [ base hspec ];
  description = "Helpers for generating clients for servant APIs in any programming language";
  license = stdenv.lib.licenses.bsd3;
}
