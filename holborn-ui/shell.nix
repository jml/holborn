{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, basic-prelude, bcrypt
      , blaze-html, bytestring, digestive-functors
      , digestive-functors-blaze, either, envparse, errors
      , postgresql-simple, servant, servant-blaze, servant-server
      , shakespeare, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
      , wai, warp, packunused
      }:
      mkDerivation {
        pname = "holborn-ui";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base basic-prelude bcrypt blaze-html bytestring
          digestive-functors digestive-functors-blaze either errors
          postgresql-simple servant servant-blaze servant-server shakespeare
          text
        ];
        executableHaskellDepends = [
          base basic-prelude blaze-html bytestring envparse servant-server
          wai warp packunused
        ];
        testHaskellDepends = [
          base basic-prelude tasty tasty-hunit tasty-quickcheck
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
