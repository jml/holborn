{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, basic-prelude, bcrypt, bytestring
      , errors, postgresql-simple, stdenv, tasty, tasty-hunit
      , tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "holborn-ui";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base basic-prelude bcrypt bytestring errors postgresql-simple text
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
