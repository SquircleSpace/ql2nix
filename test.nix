{ nixpkgs ? import <nixpkgs> {} }:
let
  qlDist = import ./qlDist.nix { fetchurl = nixpkgs.fetchurl; };
  mkNixlispBundle = import ./mkNixlispBundle.nix {
    writeTextFile = nixpkgs.writeTextFile;
    concatMapStrings = nixpkgs.lib.concatMapStrings;
    mkDerivation = nixpkgs.stdenv.mkDerivation;
    quicklisp = nixpkgs.lispPackages.quicklisp;
    clwrapper = nixpkgs.lispPackages.clwrapper;
  };
in mkNixlispBundle qlDist
