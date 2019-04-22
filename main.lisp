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

(defun produce-ql-releases (releases ql-systems)
  (with-open-file (stream #P"qlReleases.nix" :direction :output :if-exists :supersede)
    (format stream "{ fetchurl }:~%{~%")
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
          (when (gethash system ql-systems)
            (setf (gethash (concatenate 'string (system-file-name system) ".asd") allowable-system-files) t)))
        (format stream "  \"~A\" = {~%" (name release))
        (format stream "    archive = fetchurl {~%")
        (format stream "      url = \"~A\";~%" (archive-url release))
        (format stream "      sha256 = \"~A\";~%" nix-hash)
        (format stream "    };~%")
        (format stream "    name = \"~A\";~%" (name release))
        (format stream "    archiveName = \"~A\";~%" (path-name path))
        (format stream "    archiveSize = ~A;~%" (archive-size release))
        (format stream "    archiveMD5 = \"~A\";~%" (archive-md5 release))
        (format stream "    archiveContentSHA1 = \"~A\";~%" (archive-content-sha1 release))
        (format stream "    prefix = \"~A\";~%" (prefix release))
        (format stream "    systemFiles = [~%")
        (dolist (file-name (system-files release))
          (when (gethash file-name allowable-system-files)
            (format stream "      \"~A\"~%" file-name)))
        (format stream "    ];~%")
        (format stream "  };~%")))
    (format stream "}~%")))

(defun produce-ql-systems (systems)
  (with-open-file (stream #P"qlSystems.nix" :direction :output :if-exists :supersede)
    (format stream "{ qlReleases }:~%")
    (format stream "let qlSystems = {~%")
    (dolist (system systems)
      (format stream "  \"~A\" = {~%" (name system))
      (format stream "    release = qlReleases.\"~A\";~%" (name (release system)))
      (format stream "    name = \"~A\";~%" (name system))
      (format stream "    systemFileName = \"~A\";~%" (system-file-name system))
      (format stream "    requiredSystems = [~%")
      (dolist (required-system (required-systems system))
        (unless (or (equal "asdf" required-system)
                    (equal "uiop" required-system))
          ;; ASDF and UIOP will be handled separately
          (format stream "      qlSystems.\"~A\"~%" required-system)))
      (format stream "    ];~%")
      (format stream "  };~%"))
    (format stream "};~%")
    (format stream "in qlSystems~%")))

(defun produce-ql-dist ()
  (with-open-file (stream #P"qlDist.nix" :direction :output :if-exists :supersede)
    (format stream "{ fetchurl }:~%")
    (format stream "let~%")
    (format stream "  qlReleases = import ./qlReleases.nix { inherit fetchurl; };~%")
    (format stream "  qlSystems = import ./qlSystems.nix { inherit qlReleases; };~%")
    (format stream "in { inherit qlSystems qlReleases; }~%")))

(defun hash-table-keys (hash-table)
  (loop :for key :being :the :hash-keys :of hash-table :collect key))

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
        output-nixlisp-lib-files)
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

    (unless argv
      (return-from main))

    (let ((system-names argv))
      (ensure-quicklisp)

      (let ((ql-systems (make-hash-table :test 'eq))
            (ql-releases (make-hash-table :test 'eq)))
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
              (setf (gethash ql-system ql-systems) t)
              (setf (gethash (release ql-system) ql-releases) t))))

        (produce-ql-releases (hash-table-keys ql-releases) ql-systems)

        (let (systems)
          (loop :for release :being :the :hash-keys :of ql-releases :do
            (loop :for system :in (provided-systems release) :do
              (when (gethash system ql-systems)
                (push system systems))))
          (produce-ql-systems systems))

        (produce-ql-dist)))))
