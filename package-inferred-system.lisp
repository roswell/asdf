;;;; -------------------------------------------------------------------------
;;;; Package systems in the style of quick-build or faslpath

(uiop:define-package :asdf/package-inferred-system
  (:recycle :asdf/package-inferred-system :asdf/package-system :asdf)
  (:use :uiop/common-lisp :uiop
        :asdf/upgrade :asdf/session
        :asdf/component :asdf/system :asdf/system-registry :asdf/lisp-action
        :asdf/parse-defsystem)
  (:import-from :asdf/parse-defsystem
                #:parse-dependency-def)
  (:export
   #:define-file-package
   #:package-inferred-system #:sysdef-package-inferred-system-search
   #:package-system ;; backward compatibility only. To be removed.
   #:register-system-packages
   #:*defpackage-forms* #:*package-inferred-systems* #:package-inferred-system-missing-package-error))
(in-package :asdf/package-inferred-system)

(with-upgradability ()
  ;; A COMPONENT-PACKAGE-DESIGNATOR is either:
  ;; - a string-designator which names both an ASDF system and a CL package
  ;; - a two-element list (SYSTEM PACKAGE), where
  ;;   - SYSTEM is a dependency def suitable for ASDF/PARSE-DEFSYSTEM:PARSE-DEPENDENCY-DEF
  ;;   - PACKAGE is a string designator which names a package
  (defun component-package-designator-package (designator)
    (etypecase designator
      (cons (second designator))
      ((or symbol string) designator)))
  (defun component-package-designator-component (designator)
    (etypecase designator
      (cons (first designator))
      ((or symbol string) designator)))
  (defgeneric file-package-to-package (head &rest tail))
  ;; FIXME: parse `:feature' dependencies correctly, i.e. don't pass them to `uiop:define-package' unless
  ;; they're `featurep'.
  (defmacro define-file-package-to-package (head arglist)
    "Define a DEFINE-FILE-PACKAGE clause whose component-package-designators must be transformed for
UIOP:DEFINE-PACKAGE"
    `(defmethod file-package-to-package ((head (eql ',head)) &rest tail)
       (destructuring-bind ,arglist tail
         (cons head
               ;; there are actually only a very small number of different arglist shapes for a
               ;; define-file-package clause, so we just hard-code the inputs and their expansions
               ,(cond ((equal arglist '(&rest package))
                       '(mapcar #'component-package-designator-package package))
                      ((equal arglist '(package &rest symbol))
                       '(cons (component-package-designator-package package) symbol))
                      (:otherwise (error "unrecognized ASDF:DEFINE-FILE-PACKAGE clause template ~a" arglist)))))))
  (define-file-package-to-package :use (&rest package))
  (define-file-package-to-package :shadowing-import-from (package &rest symbol))
  (define-file-package-to-package :import-from (package &rest symbol))
  (define-file-package-to-package :recycle (&rest package))
  (define-file-package-to-package :mix (&rest package))
  (define-file-package-to-package :reexport (&rest package))
  (define-file-package-to-package :use-reexport (&rest package))
  (define-file-package-to-package :mix-reexport (&rest package))

  (defmacro define-file-package-passthrough (head)
    "Define a DEFINE-FILE-PACKAGE clause which may be passed as-is to UIOP:DEFINE-PACKAGE"
    `(defmethod file-package-to-package ((head (eql ',head)) &rest tail)
       (cons head tail)))
  (define-file-package-passthrough :unintern)
  (define-file-package-passthrough :shadow)
  (define-file-package-passthrough :export)
  (define-file-package-passthrough :intern)
  (define-file-package-passthrough :documentation)
  (define-file-package-passthrough :nicknames)

  (defmacro define-file-package-ignore (head)
    "Define a DEFINE-FILE-PACKAGE clause which should not be passed to UIOP:DEFINE-PACKAGE at all"
    `(defmethod file-package-to-package ((head (eql ',head)) &rest tail)
       (declare (ignorable head tail))
       nil))
  (define-file-package-ignore :depends-on)
  
  (defmethod file-package-to-package ((head (eql :local-nicknames)) &rest pairs)
    (flet ((process-pair (pair)
             (destructuring-bind (nickname package) pair
               (list nickname (component-package-designator-package package)))))
      `(:local-nicknames ,@(mapcar #'process-pair pairs))))
  
  (defmacro define-file-package (name &rest clauses)
    "Define a package for a PACKAGE-INFERRED-SYSTEM.

Like UIOP:DEFINE-PACKAGE, with extensions to handle systems whose CL package names don't match their
ASDF system names:

- Anywhere UIOP:DEFINE-PACKAGE accepts a package name, DEFINE-FILE-PACKAGE also accepts a two-element
  list (SYSTEM-NAME PACKAGE-NAME). The ASDF system for this file will depend on SYSTEM-NAME, and the package
  will IMPORT-FROM/USE/MIX/etc. PACKAGE-NAME. SYSTEM-NAME may be a string designator or a :VERSION, :FEATURE
  or :REQUIRE dependency as in a DEFSYSTEM :DEPENDS-ON clause.

- The clause (:DEPENDS-ON &rest SYSTEM-NAMES) will depend on each of the SYSTEM-NAMES without affecting
  the newly-defined package in any way.

Programmers are strongly encouraged to use string literals as system names, keywords as package names, and
uninterned symbols as symbol designators.

E.g.:
(asdf:define-file-package :test-inferred-system/package
  (:nicknames :test-inferred-system)
  (:import-from (\"bad-system\" :not-the-system-name) #:foo)
  (:depends-on \"bordeaux-threads\")
  (:use :cl
        ((:version \"need-recent-version\" \"3.2\") :need-recent-version))
  (:export #:bar))"
    `(uiop:define-package ,name ,@(remove-if #'null
                                             (loop :for clause :in clauses
                                                   :collect (apply #'file-package-to-package clause))))))

(with-upgradability ()
  ;; The names of the recognized defpackage forms.
  (defparameter *defpackage-forms* '(defpackage define-package define-file-package))

  (defun initial-package-inferred-systems-table ()
    ;; Mark all existing packages are preloaded.
    (let ((h (make-hash-table :test 'equal)))
      (dolist (p (list-all-packages))
        (dolist (n (package-names p))
          (setf (gethash n h) t)))
      h))

  ;; Mapping from package names to systems that provide them.
  (defvar *package-inferred-systems* (initial-package-inferred-systems-table))

  (defclass package-inferred-system (system)
    ()
    (:documentation "Class for primary systems for which secondary systems are automatically
in the one-file, one-file, one-system style: system names are mapped to files under the primary
system's system-source-directory, dependencies are inferred from the first defpackage form in
every such file"))

  ;; DEPRECATED. For backward compatibility only. To be removed in an upcoming release:
  (defclass package-system (package-inferred-system) ())

  ;; Is a given form recognizable as a defpackage form?
  (defun defpackage-form-p (form)
    (and (consp form)
         (member (car form) *defpackage-forms*)))

  ;; Find the first defpackage form in a stream, if any
  (defun stream-defpackage-form (stream)
    (loop :for form = (read stream nil nil) :while form
          :when (defpackage-form-p form) :return form))

  (defun file-defpackage-form (file)
    "Return the first DEFPACKAGE form in FILE."
    (with-input-file (f file)
      (stream-defpackage-form f)))

  (define-condition package-inferred-system-missing-package-error (system-definition-error)
    ((system :initarg :system :reader error-system)
     (pathname :initarg :pathname :reader error-pathname))
    (:report (lambda (c s)
               (format s (compatfmt "~@<No package form found while ~
                                     trying to define package-inferred-system ~A from file ~A~>")
                       (error-system c) (error-pathname c)))))

  (defun package-dependencies (defpackage-form)
    "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
    (assert (defpackage-form-p defpackage-form))
    (remove-duplicates
     (while-collecting (dep)
       (flet ((depend-on (package)
                ;; note: calling COMPONENT-PACKAGE-DESIGNATOR-COMPONENT on package-names provided to
                ;; CL:DEFPACKAGE and UIOP:DEFINE-PACKAGE will allow malformed invocations of those macros,
                ;; which will not be caught until we actually try to macroexpand those files.
                (dep (parse-dependency-def (component-package-designator-component package)))))
         (loop :for (option . arguments) :in (cddr defpackage-form) :do
           (ecase option
             ((:use :mix :reexport :use-reexport :mix-reexport :depends-on)
              (dolist (p arguments)
                (depend-on p)))
             ((:import-from :shadowing-import-from)
              (depend-on (first arguments)))
             #+package-local-nicknames
             ((:local-nicknames)
              (loop :for (nil actual-package-name) :in arguments :do
                (depend-on actual-package-name)))
             ((:nicknames :documentation :shadow :export :intern :unintern :recycle))))))
     :from-end t :test 'equal))

  (defun package-designator-name (package)
    "Normalize a package designator to a string"
    (etypecase package
      (package (package-name package))
      (string package)
      (symbol (string package))))

  (defun register-system-packages (system packages)
    "Register SYSTEM as providing PACKAGES."
    (let ((name (or (eq system t) (coerce-name system))))
      (dolist (p (ensure-list packages))
        (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

  (defun package-name-system (package-name)
    "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
    (check-type package-name string)
    (or (gethash package-name *package-inferred-systems*)
        (string-downcase package-name)))

  ;; Given a file in package-inferred-system style, find its dependencies
  (defun package-inferred-system-file-dependencies (file &optional system)
    (if-let (defpackage-form (file-defpackage-form file))
      (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
      (error 'package-inferred-system-missing-package-error :system system :pathname file)))

  ;; Given package-inferred-system object, check whether its specification matches
  ;; the provided parameters
  (defun same-package-inferred-system-p (system name directory subpath around-compile dependencies)
    (and (eq (type-of system) 'package-inferred-system)
         (equal (component-name system) name)
         (pathname-equal directory (component-pathname system))
         (equal dependencies (component-sideway-dependencies system))
         (equal around-compile (around-compile-hook system))
         (let ((children (component-children system)))
           (and (length=n-p children 1)
                (let ((child (first children)))
                  (and (eq (type-of child) 'cl-source-file)
                       (equal (component-name child) "lisp")
                       (and (slot-boundp child 'relative-pathname)
                            (equal (slot-value child 'relative-pathname) subpath))))))))

  ;; sysdef search function to push into *system-definition-search-functions*
  (defun sysdef-package-inferred-system-search (system-name)
  "Takes SYSTEM-NAME and returns an initialized SYSTEM object, or NIL.  Made to be added to
*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*."
    (let ((primary (primary-system-name system-name)))
      ;; this function ONLY does something if the primary system name is NOT the same as
      ;; SYSTEM-NAME.  It is used to find the systems with names that are relative to
      ;; the primary system's name, and that are not explicitly specified in the system
      ;; definition
      (unless (equal primary system-name)
        (let ((top (find-system primary nil)))
          (when (typep top 'package-inferred-system)
            (if-let (dir (component-pathname top))
              (let* ((sub (subseq system-name (1+ (length primary))))
                     (component-type (class-for-type top :file))
                     (file-type (file-type (make-instance component-type)))
                     (f (probe-file* (subpathname dir sub :type file-type)
                                     :truename *resolve-symlinks*)))
                (when (file-pathname-p f)
                  (let ((dependencies (package-inferred-system-file-dependencies f system-name))
                        (previous (registered-system system-name))
                        (around-compile (around-compile-hook top)))
                    (if (same-package-inferred-system-p previous system-name dir sub around-compile dependencies)
                        previous
                        (eval `(defsystem ,system-name
                                 :class package-inferred-system
                                 :default-component-class ,component-type
                                 :source-file ,(system-source-file top)
                                 :pathname ,dir
                                 :depends-on ,dependencies
                                 :around-compile ,around-compile
                                 :components ((,component-type file-type :pathname ,sub)))))))))))))))

(with-upgradability ()
  (pushnew 'sysdef-package-inferred-system-search *system-definition-search-functions*)
  (setf *system-definition-search-functions*
        (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
                *system-definition-search-functions*)))
