{ mkDerivation, aeson, base, bytestring, bytestring-conversion
, hspec, hspec-wai, http-types, QuickCheck, servant, servant-server
, transformers, wai, warp
, forked-servant
, stdenv
}:
mkDerivation rec {
  pname = "servant-mock";
  version = "0.7.1";
  src = forked-servant;
  preCompileBuildDriver = ''
    cd ${pname}/
  '';
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring http-types QuickCheck servant servant-server
    transformers wai
  ];
  executableHaskellDepends = [
    aeson base QuickCheck servant-server warp
  ];
  testHaskellDepends = [
    aeson base bytestring-conversion hspec hspec-wai QuickCheck servant
    servant-server wai
  ];
  homepage = "http://github.com/haskell-servant/servant";
  description = "Derive a mock server for free from your servant API types";
  license = stdenv.lib.licenses.bsd3;
}
