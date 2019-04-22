{ writeTextFile, concatMapStrings, mkDerivation, quicklisp, clwrapper }:
qlDist:
let
  nixlispDist = import ./nixlispDist.nix { inherit writeTextFile concatMapStrings mkDerivation qlDist; };
  nixlispBundle = import ./nixlispBundle.nix { inherit nixlispDist quicklisp clwrapper writeTextFile mkDerivation; };
in nixlispBundle
