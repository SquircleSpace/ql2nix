{ writeTextFile, lib, stdenv, lispPackages, ... }:
qlDist:
let
  concatMapStrings = lib.concatMapStrings;
  mkDerivation = stdenv.mkDerivation;
  quicklisp = lispPackages.quicklisp;
  clwrapper = lispPackages.clwrapper;
  nixlispDist = import ./nixlispDist.nix {
    inherit writeTextFile concatMapStrings mkDerivation qlDist;
  };
  nixlispBundle = import ./nixlispBundle.nix {
    inherit nixlispDist quicklisp clwrapper writeTextFile mkDerivation;
  };
in nixlispBundle
