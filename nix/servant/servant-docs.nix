{ mkDerivation, aeson, aeson-pretty, base, bytestring
, bytestring-conversion, case-insensitive, control-monad-omega
, hashable, hspec, http-media, http-types, lens, servant
, string-conversions, text, unordered-containers
, forked-servant
, stdenv
}:
mkDerivation rec {
  pname = "servant-docs";
  version = "0.7.1";
  src = forked-servant;
  preCompileBuildDriver = ''
    cd ${pname}/
  '';
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring bytestring-conversion
    case-insensitive control-monad-omega hashable http-media http-types
    lens servant string-conversions text unordered-containers
  ];
  executableHaskellDepends = [
    aeson base bytestring-conversion lens servant string-conversions
    text
  ];
  testHaskellDepends = [
    aeson base hspec lens servant string-conversions
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "generate API docs for your servant webservice";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = [ "x86_64-darwin" "x86_64-linux" ];
}
