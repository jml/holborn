{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, http-types, lens, mainland-pretty, purescript-bridge
, servant, servant-foreign, servant-server, servant-subscriber
, stdenv, text
}:
mkDerivation {
  pname = "servant-purescript";
  version = "0.2.0.1";
  sha256 = "0y71n46h3dh2yph01c2nh4hj6jahyfzn8qqdhs01fjj1bdl3068n";
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath http-types lens
    mainland-pretty purescript-bridge servant servant-foreign
    servant-server servant-subscriber text
  ];
  testHaskellDepends = [
    aeson base containers lens mainland-pretty purescript-bridge
    servant servant-foreign servant-subscriber text
  ];
  homepage = "https://github.com/eskimor/servant-purescript#readme";
  description = "Generate PureScript accessor functions for you servant API";
  license = stdenv.lib.licenses.bsd3;
}
