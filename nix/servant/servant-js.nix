{ mkDerivation, aeson, base, base-compat, charset, filepath, hspec
, hspec-expectations, language-ecmascript, lens, servant
, servant-foreign, servant-server, stm, text, transformers, warp
, forked-servant
, stdenv
}:
mkDerivation rec {
  pname = "servant-js";
  version = "0.7.1";
  src = forked-servant;
  preCompileBuildDriver = ''
    cd ${pname}/
  '';
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat charset lens servant-foreign text
  ];
  executableHaskellDepends = [
    aeson base filepath lens servant servant-server stm transformers
    warp
  ];
  testHaskellDepends = [
    base base-compat hspec hspec-expectations language-ecmascript lens
    servant text
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Automatically derive javascript functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
