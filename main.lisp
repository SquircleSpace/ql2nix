;; Copyright 2019 Bradley Jensen
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :ql2nix
  (:use :common-lisp)
  (:import-from :uiop)
  (:export #:main))
(in-package :ql2nix)

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))

(defvar *nixlisp-bundle-contents*
  (load-time-value
   (with-open-file (stream (uiop:merge-pathnames* #P"nixlispBundle.nix" *load-truename*))
     (uiop:slurp-input-stream :string stream))))

(defvar *nixlisp-dist-contents*
  (load-time-value
   (with-open-file (stream (uiop:merge-pathnames* #P"nixlispDist.nix" *load-truename*))
     (uiop:slurp-input-stream :string stream))))

(defvar *mk-nixlisp-bundle-contents*
  (load-time-value
   (with-open-file (stream (uiop:merge-pathnames* #P"mkNixlispBundle.nix" *load-truename*))
     (uiop:slurp-input-stream :string stream))))

(defun produce-nixlisp-lib-files ()
  (with-open-file (stream #P"nixlispDist.nix" :direction :output :if-exists :supersede)
    (write-string *nixlisp-dist-contents* stream))
  (with-open-file (stream #P"nixlispBundle.nix" :direction :output :if-exists :supersede)
    (write-string *nixlisp-bundle-contents* stream))
  (with-open-file (stream #P"mkNixlispBundle.nix" :direction :output :if-exists :supersede)
    (write-string *mk-nixlisp-bundle-contents* stream)))

(defmacro define-wrapper (function-name package-name)
  `(defun ,function-name (object)
     (funcall (find-symbol ,(symbol-name function-name) ,package-name) object)))

(define-wrapper ensure-local-archive-file :ql-dist)
(define-wrapper name :ql-dist)
(define-wrapper archive-url :ql-dist)
(define-wrapper archive-size :ql-dist)
(define-wrapper archive-md5 :ql-dist)
(define-wrapper archive-content-sha1 :ql-dist)
(define-wrapper prefix :ql-dist)
(define-wrapper system-files :ql-dist)
(define-wrapper release :ql-dist)
(define-wrapper system-file-name :ql-dist)
(define-wrapper required-systems :ql-dist)
(define-wrapper provided-releases :ql-dist)
(define-wrapper provided-systems :ql-dist)
(define-wrapper find-system :ql-dist)
(define-wrapper quickload :ql)

(defun ensure-quicklisp ()
  (loop
    (if (find-package :quicklisp)
        (return)
        (restart-case
            (error "You must load quicklisp before continuing")
          (check-again ())))))

(defmacro equal-case (key &body clauses)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,key))
       (cond
         ,@(loop :for clause :in clauses :collect
           `(,(if (eq 'otherwise (car clause))
                  `t
                  `(equal ,value ,(car clause)))
             ,@(cdr clause)))))))

(defun nix-prefetch (path)
  (destructuring-bind (hash store-path)
      (uiop:run-program `("nix-prefetch-url" "--print-path" ,(concatenate 'string "file://" (uiop:unix-namestring path)))
                        :output :lines)
    (values hash store-path)))

(defun path-name (pathname)
  (make-pathname :name (pathname-name pathname) :type (pathname-type pathname)))

(defvar *indent-level* 0)

(defmacro with-indent (&body body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defun indent-format (output format-specifier &rest args)
  (dotimes (count *indent-level*)
    (format output "  "))
  (apply 'format output format-specifier args))

(defun produce-ql-releases (releases ql-systems &key (output-stream *standard-output*))
  (setf releases (sort (copy-list releases) 'string< :key 'name))
  (indent-format output-stream "{~%")
  (with-indent
    (dolist (release releases)
      (let* ((path (ensure-local-archive-file release))
             (nix-hash (nix-prefetch path))
             (allowable-system-files (make-hash-table :test 'equal)))
        ;; We only want the ultimate nixlisp dist closure to include
        ;; systems that we... well... captured in the closure.  asd
        ;; files that weren't implicated as part of building the
        ;; closure shouldn't be included in the list.  Otherwise,
        ;; quicklisp will try to find a system corresponding to the
        ;; asd and get confused.  If we don't mention the asd and we
        ;; don't mention any systems contained within the asd then
        ;; quicklisp (hopefully) won't even notice it.
        (dolist (system (provided-systems release))
          (when (gethash (name system) ql-systems)
            (setf (gethash (concatenate 'string (system-file-name system) ".asd") allowable-system-files) t)))
        (indent-format output-stream "\"~A\" = {~%" (name release))
        (with-indent
          (indent-format output-stream "archive = fetchurl {~%")
          (with-indent
            (indent-format output-stream "url = \"~A\";~%" (archive-url release))
            (indent-format output-stream "sha256 = \"~A\";~%" nix-hash))
          (indent-format output-stream "};~%")
          (indent-format output-stream "name = \"~A\";~%" (name release))
          (indent-format output-stream "archiveName = \"~A\";~%" (path-name path))
          (indent-format output-stream "archiveSize = ~A;~%" (archive-size release))
          (indent-format output-stream "archiveMD5 = \"~A\";~%" (archive-md5 release))
          (indent-format output-stream "archiveContentSHA1 = \"~A\";~%" (archive-content-sha1 release))
          (indent-format output-stream "prefix = \"~A\";~%" (prefix release))
          (indent-format output-stream "systemFiles = [~%")
          (with-indent
            (dolist (file-name (system-files release))
              (when (gethash file-name allowable-system-files)
                (indent-format output-stream "\"~A\"~%" file-name))))
          (indent-format output-stream "];~%"))
        (indent-format output-stream "};~%"))))
  (indent-format output-stream "};~%"))

(defun produce-ql-systems (systems &key (output-stream *standard-output*))
  (setf systems (sort (copy-list systems) 'string< :key 'name))
  (indent-format output-stream "{~%")
  (with-indent
    (dolist (system systems)
      (indent-format output-stream "\"~A\" = {~%" (name system))
      (with-indent
        (indent-format output-stream "release = qlReleases.\"~A\";~%" (name (release system)))
        (indent-format output-stream "name = \"~A\";~%" (name system))
        (indent-format output-stream "systemFileName = \"~A\";~%" (system-file-name system))
        (indent-format output-stream "requiredSystems = [~%")
        (with-indent
          (dolist (required-system (required-systems system))
            (unless (or (equal "asdf" required-system)
                        (equal "uiop" required-system))
              ;; ASDF and UIOP will be handled separately
              (indent-format output-stream "qlSystems.\"~A\"~%" required-system))))
        (indent-format output-stream "];~%"))
      (indent-format output-stream "};~%")))
  (indent-format output-stream "};~%"))

(defun produce-ql-dist (ql-releases ql-systems)
  (with-open-file (stream #P"qlDist.nix" :direction :output :if-exists :supersede)
    (indent-format stream "# This file is machine generated.  Do not edit it!~%")
    (indent-format stream "{ fetchurl }:~%")
    (indent-format stream "let~%")
    (with-indent
      (indent-format stream "qlReleases =~%")
      (with-indent
        (produce-ql-releases (hash-table-values ql-releases) ql-systems :output-stream stream))
      (indent-format stream "qlSystems =~%")
      (with-indent
        (produce-ql-systems (hash-table-values ql-systems) :output-stream stream)))
    (indent-format stream "in { inherit qlSystems qlReleases; }~%")))

(defun hash-table-values (hash-table)
  (loop :for value :being :the :hash-values :of hash-table :collect value))

(defvar *touched-systems* (make-hash-table :test 'eq))

(defmethod asdf:perform :before (op (system asdf:system))
  (setf (gethash system *touched-systems*) t))

(defvar *help*
  "Usage: ql2nix --quicklisp-setup <PATH> [options] [--] [systems...]

ql2nix uses QL:QUICKLOAD to load the named systems.  Any Quicklisp
system that gets touched by ASDF will be captured by ql2nix.  After
loading all the systems, ql2nix will output a Nix file describing the
captured Quicklisp systems.  Using the mkNixlispBundle Nix function,
you can easily obtain a derivation which includes all of the Quicklisp
packages captured in the closure.  The derivation also includes a
Common Lisp source file that, when loaded, makes the systems in the
closure available to ASDF.

Options:

--quicklisp-setup <PATH>
  Must be specified exactly once.  Pass in the path to Quicklisp's
  setup.lisp file.  ql2nix will use the given Quicklisp installation
  when quickloading dependencies.

--project-dir <PATH>
  Add PATH to ASDF's source registry.  In addition, ql2nix will ignore
  any systems contained within PATH.  If you pass in the path to your
  project and then have ql2nix quickload your system then ql2nix will
  automatically discover all of your project's dependencies!  Nifty!

--load <PATH>
  Load the file at the given path.  If this file uses Quicklisp (or
  ASDF) to load a system that Quicklisp provides then the loaded
  system will be included in the closure.  You can also use this
  option to trigger side effects.  For example, you can add something
  to *FEATURES* or modify state that will impact how the systems are
  loaded.

--eval <EXPR>
  Evaluate the given Common Lisp form.  Like the --load option, this
  option allows you to modify state or load systems in specialized
  ways.

--system-file
  Open the given text file and treat every line of the file as a
  system to quickload.  This option gives you a way to avoid naming a
  lot of systems on the command line.

--nixlisp-lib
  Write out the supporting nixlisp Nix files required to make use of
  ql2nix's system closures.  Until ql2nix's mkNixlispBundle function
  gets integrated into nixpkgs, you will probably want to use this
  option!

--help
  Show this text and exit.
")

(defun help ()
  (write-string *help* *error-output*))

(defun main (&optional (argv (uiop:raw-command-line-arguments)))
  (pop argv)
  (let (project-paths
        quicklisp-setup-path
        output-nixlisp-lib-files
        eval-forms
        system-file-paths)
    (loop :while argv :for arg = (pop argv) :do
      (equal-case arg
        ("--quicklisp-setup"
         (setf quicklisp-setup-path (pop argv))
         (unless quicklisp-setup-path
           (error "--quicklisp-setup requires a path to Quicklisp's setup.lisp file")))

        ("--project-dir"
         (let ((project-path (pop argv)))
           (unless project-path
             (error "Missing required argument: path to project source"))
           (setf project-path (truename (uiop:ensure-directory-pathname (uiop:merge-pathnames* (uiop:parse-native-namestring project-path)))))
           (assert (uiop:absolute-pathname-p project-path))
           (push project-path asdf:*central-registry*)))

        ("--nixlisp-lib"
         (setf output-nixlisp-lib-files t))

        ("--load"
         (let ((path (pop argv)))
           (unless path
             (error "Missing required argument: path to load"))
           (push `(load ,path) eval-forms)))

        ("--eval"
         (let ((form-string (pop argv)))
           (unless form-string
             (error "Missing required argument: form to eval"))
           (push (read (make-string-input-stream form-string)) eval-forms)))

        ("--system-file"
         (let ((path (pop argv)))
           (unless path
             (error "Missing required argument: path to system file"))
           (push path system-file-paths)))

        ("--help"
         (help)
         (return-from main))

        ("--"
         (return))

        (otherwise
         (push arg argv)
         (return))))

    (when quicklisp-setup-path
      (if (find-package :quicklisp)
          (error "Quicklisp is already loaded")
          (progn
            (load quicklisp-setup-path)
            (ensure-quicklisp))))

    (when output-nixlisp-lib-files
      (produce-nixlisp-lib-files))

    (dolist (form (nreverse eval-forms))
      (eval form))

    (let ((system-names argv))
      (dolist (system-file-path system-file-paths)
        (with-open-file (stream system-file-path)
          (loop :for line = (readline stream nil :eof) :while line :do
            (push line system-names))))

      (unless system-names
        (return-from main))

      (ensure-quicklisp)

      (let ((ql-systems (make-hash-table :test 'equal))
            (ql-releases (make-hash-table :test 'equal)))
        (dolist (system-name system-names)
          (quickload system-name))

        (loop :for system :being :the :hash-keys :of *touched-systems* :do
          (block continue
            (dolist (project-path project-paths)
              (when (uiop:subpathp (asdf:component-pathname system) project-path)
                (return-from continue)))
            (let* ((system-name (asdf:component-name system))
                   (ql-system (or (find-system system-name)
                                  (progn
                                    (warn "Couldn't find quicklisp system for ASDF system: ~A" system-name)
                                    (return-from continue)))))
              (setf (gethash (name ql-system) ql-systems) ql-system)
              (setf (gethash (name (release ql-system)) ql-releases) (release ql-system)))))

        (produce-ql-dist ql-releases ql-systems)))))
