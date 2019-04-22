{ pkgs ? import <nixpkgs> {} }:
let
  build = pkgs.writeTextFile {
    name = "ql2nix-build.lisp";
    text = ''
      (require '#:asdf)
      (let* ((asdf:*central-registry* (cons (truename ".") asdf:*central-registry*))
             (system (asdf:find-system '#:ql2nix))
             (uiop:*image-dump-hook* (cons (lambda () (asdf:clear-system system)) uiop:*image-dump-hook*)))
        (asdf:oos 'asdf:program-op system))
    '';
  };
  clwrapper = pkgs.lispPackages.clwrapper;
in
pkgs.stdenv.mkDerivation rec {
  name = "ql2nix-${version}";
  version = "1.0.0";
  src = ./.;
  buildInputs = [
    clwrapper
  ];
  buildPhase = ''
    ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"$(pwd)\"))"
    export ASDF_OUTPUT_TRANSLATIONS
    NIX_LISP_SKIP_CODE=1 source "${clwrapper}/bin/common-lisp.sh" || true
    "${clwrapper}/bin/common-lisp.sh" "$NIX_LISP_LOAD_FILE" "${build}"
  '';
  installPhase = ''
    mkdir -p "$out/bin"
    cp ql2nix "$out/bin"
  '';
  dontStrip = true;
}
