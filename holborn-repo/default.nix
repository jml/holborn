{ mkDerivation, base, basic-prelude, either, http-types, pipes
, pipes-shell, pipes-wai, servant, servant-server, stdenv, wai
, warp
}:
mkDerivation {
  pname = "holborn-repo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude either http-types pipes pipes-shell pipes-wai
    servant servant-server wai warp
  ];
  license = stdenv.lib.licenses.unfree;
}
