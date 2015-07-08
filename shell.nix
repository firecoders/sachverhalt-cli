{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, network, stdenv, transformers, cabal-install,
        aeson }:
      mkDerivation {
        pname = "sachverhalt-cli";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ base network transformers aeson ];
        buildTools = [ cabal-install ];
        homepage = "https://github.com/firecoders/sachverhalt-cli";
        description = "The client-side reference implementation of the sachverhalt protocol";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
