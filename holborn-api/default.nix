{ mkDerivation, aeson, base, basic-prelude, containers, either
, servant, servant-server, stdenv, wai, warp
}:
mkDerivation {
  pname = "holborn-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base basic-prelude containers either servant servant-server
    wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
