{ nixpkgs ? import <nixpkgs> {} }:
let
  qlReleases = import ./qlReleases.nix { fetchurl = nixpkgs.fetchurl; };
  qlSystems = import ./qlSystems.nix { qlReleases = qlReleases; };
in
import ./nixlispDist.nix {
  writeTextFile = nixpkgs.writeTextFile;
  attrValues = nixpkgs.lib.attrValues;
  concatMapStrings = nixpkgs.lib.concatMapStrings;
  mkDerivation = nixpkgs.stdenv.mkDerivation;
  qlReleases = qlReleases;
  qlSystems = qlSystems;
}
