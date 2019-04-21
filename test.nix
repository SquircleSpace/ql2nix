{ nixpkgs ? import <nixpkgs> {} }:
let
  qlReleases = import ./qlReleases.nix { fetchurl = nixpkgs.fetchurl; };
  qlSystems = import ./qlSystems.nix { qlReleases = qlReleases; };
  nixlispDist = import ./nixlispDist.nix {
    writeTextFile = nixpkgs.writeTextFile;
    attrValues = nixpkgs.lib.attrValues;
    concatMapStrings = nixpkgs.lib.concatMapStrings;
    mkDerivation = nixpkgs.stdenv.mkDerivation;
    qlReleases = qlReleases;
    qlSystems = qlSystems;
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
