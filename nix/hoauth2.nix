{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, http-conduit, http-types, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "hoauth2";
  version = "0.5.3.1";
  src = fetchgit {
    url = "https://github.com/teh/hoauth2";
    sha256 = "08ah3d64nb3c6v5p53jphkxay33wwabg9ban13xm89vahzsr23n9";
    rev = "d622f48";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-conduit http-types text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers http-conduit http-types text wai
    warp
  ];
  homepage = "https://github.com/freizl/hoauth2";
  description = "Haskell OAuth2 authentication client";
  license = stdenv.lib.licenses.bsd3;
}
