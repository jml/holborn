{ mkDerivation, aeson, base, base64-bytestring, bytestring, cookie
, entropy, errors, hashable, hoauth2, holborn-prelude, http-client
, http-client-tls, http-reverse-proxy, http-types, jose
, network-uri, optparse-applicative, servant, servant-server
, stdenv, stm, tasty, tasty-hspec, tasty-hunit, tasty-quickcheck
, text, transformers, unordered-containers, wai, wai-extra, warp
, warp-tls
}:
mkDerivation {
  pname = "holborn-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring cookie entropy errors
    hashable hoauth2 holborn-prelude http-client http-reverse-proxy
    http-types jose network-uri servant servant-server stm text
    transformers unordered-containers wai warp
  ];
  executableHaskellDepends = [
    base holborn-prelude http-client http-client-tls network-uri
    optparse-applicative wai-extra warp warp-tls
  ];
  testHaskellDepends = [
    base holborn-prelude tasty tasty-hspec tasty-hunit tasty-quickcheck
  ];
  description = "Reverse proxy / http terminator for holborn";
  license = stdenv.lib.licenses.unfree;
}
