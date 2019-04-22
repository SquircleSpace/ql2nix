{ nixpkgs ? import <nixpkgs> {} }:
let
  qlDist = import ./qlDist.nix { fetchurl = nixpkgs.fetchurl; };
  nixlispDist = import ./nixlispDist.nix {
    writeTextFile = nixpkgs.writeTextFile;
    concatMapStrings = nixpkgs.lib.concatMapStrings;
    mkDerivation = nixpkgs.stdenv.mkDerivation;
    inherit qlDist;
  };
  nixlispBundle = import ./nixlispBundle.nix {
    nixlispDist = nixlispDist;
    quicklisp = nixpkgs.lispPackages.quicklisp;
    clwrapper = nixpkgs.lispPackages.clwrapper;
    writeTextFile = nixpkgs.writeTextFile;
    mkDerivation = nixpkgs.stdenv.mkDerivation;
  };
in
nixlispBundle
