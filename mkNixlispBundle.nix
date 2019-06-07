{ writeTextFile, fetchurl, lib, stdenv, lispPackages, ... }:
qlDist:
let
  concatMapStrings = lib.concatMapStrings;
  mkDerivation = stdenv.mkDerivation;
  quicklisp = lispPackages.quicklisp;
  clwrapper = lispPackages.clwrapper;
  nixlispDist = import ./nixlispDist.nix {
    inherit writeTextFile concatMapStrings mkDerivation;
    qlDist = qlDist { inherit fetchurl; };
  };
  nixlispBundle = import ./nixlispBundle.nix {
    inherit nixlispDist quicklisp clwrapper writeTextFile mkDerivation;
  };
in nixlispBundle
