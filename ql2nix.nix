# Copyright 2019 Bradley Jensen
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{ pkgs ? import <nixpkgs> {} }: pkgs.callPackage (
{ lib, stdenv, makeWrapper, writeTextFile, clwrapper, lispName ? null }:
let
  lispName' = if lispName == null then clwrapper.lisp.pname else lispName;
  build = writeTextFile {
    name = "ql2nix-build.lisp";
    text = ''
      (require '#:asdf)
      (let* ((asdf:*central-registry* (cons (truename ".") asdf:*central-registry*))
             (system (asdf:find-system '#:ql2nix))
             (uiop:*image-dump-hook* (cons (lambda () (asdf:clear-system system)) uiop:*image-dump-hook*)))
        (asdf:oos 'asdf:program-op system))
    '';
  };
in
stdenv.mkDerivation {
  pname = "ql2nix";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [
    makeWrapper
  ];
  buildInputs = [
    clwrapper
  ];
  buildPhase = ''
    runHook preBuild
    ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"$(pwd)\"))"
    export ASDF_OUTPUT_TRANSLATIONS
    NIX_LISP_SKIP_CODE=1 source "${clwrapper}/bin/common-lisp.sh" || true
    "${clwrapper}/bin/common-lisp.sh" "$NIX_LISP_LOAD_FILE" "${build}"
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin"
    cp ql2nix "$out/bin"
    runHook postInstall
  '';
  preFixup = lib.optionalString (lispName' == "sbcl") ''
    wrapProgram "$out/bin/ql2nix" \
      --set SBCL_HOME ${lib.escapeShellArg "${clwrapper.lisp}/lib/sbcl"}
  '';
  dontStrip = true;
}
) { inherit (pkgs.lispPackages) clwrapper; }
