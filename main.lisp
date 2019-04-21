(defpackage :ql2nix/main
  (:use :common-lisp)
  (:import-from :uiop)
  (:export #:main))
(in-package :ql2nix/main)

(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))

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

(defun nix-safe-name (name)
  ;; TODO
  name)

(defun path-name (pathname)
  (make-pathname :name (pathname-name pathname) :type (pathname-type pathname)))

(defun produce-ql-releases (releases)
  (with-open-file (stream #P"qlReleases.nix" :direction :output :if-exists :supersede)
    (format stream "{ fetchurl }:~%{~%")
    (dolist (release releases)
      (let* ((path (ql-dist:ensure-local-archive-file release))
             (nix-hash (nix-prefetch path)))
        (format stream "  ~A = {~%" (nix-safe-name (ql-dist:name release)))
        (format stream "    archive = fetchurl {~%")
        (format stream "      url = \"~A\";~%" (ql-dist:archive-url release))
        (format stream "      sha256 = \"~A\";~%" nix-hash)
        (format stream "    };~%")
        (format stream "    name = \"~A\";~%" (ql-dist:name release))
        (format stream "    archiveName = \"~A\";~%" (path-name path))
        (format stream "    archiveSize = ~A;~%" (ql-dist:archive-size release))
        (format stream "    archiveMD5 = \"~A\";~%" (ql-dist:archive-md5 release))
        (format stream "    archiveContentSHA1 = \"~A\";~%" (ql-dist:archive-content-sha1 release))
        (format stream "    prefix = \"~A\";~%" (ql-dist:prefix release))
        (format stream "    systemFiles = [~%")
        (dolist (file-name (ql-dist:system-files release))
          (format stream "      \"~A\"~%" file-name))
        (format stream "    ];~%")
        (format stream "  };~%")))
    (format stream "}~%")))

(defun produce-ql-systems (systems)
  (with-open-file (stream #P"qlSystems.nix" :direction :output :if-exists :supersede)
    (format stream "{ qlReleases }:~%")
    (format stream "let qlSystems = {~%")
    (dolist (system systems)
      (format stream "  ~A = {~%" (nix-safe-name (ql-dist:name system)))
      (format stream "    release = qlReleases.~A;~%" (nix-safe-name (ql-dist:name (ql-dist:release system))))
      (format stream "    name = \"~A\";~%" (ql-dist:name system))
      (format stream "    systemFileName = \"~A\";~%" (ql-dist:system-file-name system))
      (format stream "    requiredSystems = [~%")
      (dolist (required-system (ql-dist:required-systems system))
        (format stream "      qlSystems.~A~%" (nix-safe-name required-system)))
      (format stream "    ];~%")
      (format stream "  };~%"))
    (format stream "};~%")
    (format stream "in qlSystems~%")))

(defun produce-releases-in-dist (dist)
  (produce-ql-releases (ql-dist:provided-releases dist)))

(defun hash-table-keys (hash-table)
  (loop :for key :being :the :hash-keys :of hash-table :collect key))

(defvar *touched-systems* (make-hash-table :test 'eq))

(defmethod asdf:perform :before (op (system asdf:system))
  (setf (gethash system *touched-systems*) t))

(defun main (&optional (argv (uiop:raw-command-line-arguments)))
  (let (project-paths)
    (loop :while argv :for arg = (pop argv) :do
      (equal-case arg
        ("--quicklisp-setup"
         (if (find-package :quicklisp)
             (error "Quicklisp is already loaded")
             (progn
               (load (pop argv))
               (ensure-quicklisp))))

        ("--project-dir"
         (let ((project-path (pop argv)))
           (unless project-path
             (error "Missing required argument: path to project source"))
           (setf project-path (truename (uiop:ensure-directory-pathname (uiop:merge-pathnames* (uiop:parse-native-namestring project-path)))))
           (assert (uiop:absolute-pathname-p project-path))
           (push project-path asdf:*central-registry*)))

        ("--"
         (return))
        
        (otherwise
         (push arg argv)
         (return))))

    (ensure-quicklisp)
    (let ((system-names argv))
      (unless system-names
        (error "Missing required argument: the name of a system to load"))

      (let ((ql-systems (make-hash-table :test 'eq))
            (ql-releases (make-hash-table :test 'eq)))
        (dolist (system-name system-names)
          (asdf:load-system system-name))

        (loop :for system :being :the :hash-keys :of *touched-systems* :do
          (block continue
            (dolist (project-path project-paths)
              (when (uiop:subpathp (asdf:component-pathname system) project-path)
                (return-from continue)))
            (let* ((system-name (asdf:component-name system))
                   (ql-system (or (ql-dist:find-system system-name)
                                  (progn
                                    (warn "Couldn't find quicklisp system for ASDF system: ~A" system-name)
                                    (return-from continue)))))
              (setf (gethash ql-system ql-systems) t)
              (setf (gethash (ql-dist:release ql-system) ql-releases) t))))

        (produce-ql-releases (hash-table-keys ql-releases))
        
        (let (systems)
          (loop :for release :being :the :hash-keys :of ql-releases :do
            (loop :for system :in (ql-dist:provided-systems release) :do
              (when (gethash system ql-systems)
                (push system systems))))
          (produce-ql-systems systems))))))
