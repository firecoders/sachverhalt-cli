{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, aeson, base, bytestring, lens, lens-aeson
      , mtl, network, stdenv, text, transformers, unordered-containers
      }:
      mkDerivation {
        pname = "sachverhalt-cli";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ cabal-install
          aeson base bytestring lens mtl network text transformers
          unordered-containers
          (lens-aeson.overrideDerivation (attrs: {
            doCheck = false;
          }))
        ];
        homepage = "https://github.com/firecoders/sachverhalt-cli";
        description = "The client-side reference implementation of the sachverhalt protocol";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
