{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, http-conduit, http-types, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "hoauth2";
  version = "0.5.3.1";
  src = fetchgit {
    url = "https://github.com/teh/hoauth2";
    sha256 = "10nl0lf4bxx8qjkhqz33mmra2fwqf37kxy9y44jj0smyqbkhqdah";
    rev = "0f8a9b7fb0a644b14fc73598486d43301a3ae879";
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
