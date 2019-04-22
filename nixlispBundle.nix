{ nixlispDist, quicklisp, clwrapper, writeTextFile, mkDerivation }:
let
  bundler = writeTextFile {
    name = "bundler.lisp";
    text = ''
      (setf *debugger-hook* (lambda (&rest args) (declare (ignore args)) (sb-debug:print-backtrace) (uiop:quit 1)))

      (eval-when (:compile-toplevel :load-toplevel :execute)
        (load "quicklisp/setup.lisp"))

      (ql:bundle-systems (mapcar 'ql-dist:name (ql-dist:provided-systems (ql-dist:find-dist "nixlisp")))
                         :to #P"bundle/")

      (uiop:quit 0)
    '';
  };
in mkDerivation rec {
  name = "nixlisp-bundle-${version}";
  version = "1.0.0";
  unpackPhase = "true";
  bundleIt = ''
  '';
  buildPhase = ''
    mkdir -p quicklisp/tmp
    mkdir -p quicklisp/local-projects
    mkdir -p quicklisp/dists/nixlisp/archives
    mkdir -p quicklisp/quicklisp

    cp -r "${quicklisp}/lib/common-lisp/quicklisp/"* quicklisp/

    # We have our own dists, thank you very much
    if [ -d quicklisp/dists/quicklisp ]; then
      rm -rdf quicklisp/dists/quicklisp
    fi

    ${nixlispDist}/bin/nixlisp-installer quicklisp/dists/nixlisp/

    ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t :here))"
    export ASDF_OUTPUT_TRANSLATIONS
    NIX_LISP_SKIP_CODE=1 source "${clwrapper}/bin/common-lisp.sh" || true
    echo "${clwrapper}/bin/common-lisp.sh" "$NIX_LISP_LOAD_FILE" "${bundler}" > command.txt
    mkdir -p bundle
    "${clwrapper}/bin/common-lisp.sh" "$NIX_LISP_LOAD_FILE" "${bundler}"
  '';
  installPhase = ''
    mkdir -p "$out/lib/common-lisp/nixlispBundle"
    tar -c -C bundle/ . | tar -x -C "$out/lib/common-lisp/nixlispBundle/"
    # This file contains nondeterministic output (e.g. date and info
    # about host).  Until we have some reason to include it (or a
    # replacement), its better to leave it out.
    rm "$out/lib/common-lisp/nixlispBundle/bundle-info.sexp"
  '';
}
