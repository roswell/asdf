(uiop/package:define-package :uiop/version
    (:recycle :uiop/version :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:*uiop-version*
   #:make-version
   #:version-object #:version-string-invalid-error #:version-pre-release-p #:version-pre-release-for
   #:version-next #:version-string
   #:semantic-version #:default-version
   #:version-constraint-satisfied-p
   #:parse-version #:unparse-version #:version< #:version<= #:version= ;; version support, moved from uiop/utility
   #:next-version
   #:deprecated-function-condition #:deprecated-function-name ;; deprecation control
   #:deprecated-function-style-warning #:deprecated-function-warning
   #:deprecated-function-error #:deprecated-function-should-be-deleted
   #:version-deprecation #:with-deprecation))
(in-package :uiop/version)

(with-upgradability ()
  (defparameter *fake-uiop-version* "3.4.0"
    "ASDF has some logic in FIND-SYSTEM to ensure that an old version of UIOP
isn't returned. To do so, it looks at form '(2 2 2) in this file. Because old
versions of ASDF can't parse version strings with pre-release information, we
make sure this is always set to the non-pre-release version.")
  (defparameter *uiop-version* "3.4.0-alpha.1"))

;; Base version-object class and API.
(with-upgradability ()
  ;; Introduced in version 3.4. This can't be called VERSION because ASDF
  ;; already exports a VERSION (a slot name for COMPONENT, why???). So let's
  ;; not deal with changing VERSION's home package and all the messiness that
  ;; would entail.
  (defclass version-object ()
    ()
    (:documentation
     "The base class of version specifiers."))

  (defmethod print-object ((version version-object) stream)
    (print-unreadable-object (version stream :type t :identity t)
      (format stream "~A" (version-string version))))

  (define-condition version-string-invalid-error (error)
    ((%version-string
      :initarg :version-string
      :reader %version-string)
     (reason
      :initarg :reason
      :reader %reason))
    (:report
     (lambda (condition stream)
       (format stream "The version string ~S is valid.~@[~%~A~]"
               (%version-string condition) (%reason condition)))))

  (defun make-version (version-string &key (class 'default-version))
    (when (not (null version-string))
      (restart-case
          (make-instance class :version-string version-string)
        (continue ()
          :report "Return NIL as the version object."
          nil)
        (use-value (new-version-string)
          :report "Retry parsing, with a new version string."
          :interactive (lambda ()
                         (format *query-io* "Enter a new version string: ")
                         (list (read-line *query-io*)))
          (make-version new-version-string :class class)))))

  (defun ensure-version (maybe-version &key (class 'default-version))
    (if (stringp maybe-version)
        (make-version maybe-version :class class)
        maybe-version))
  (defgeneric version-next (version)
    (:documentation "Returns the next version after VERSION. The returned
version must not be a pre-release.")
    (:method ((version null))
      nil)
    (:method ((version string))
      (version-next (make-version version))))
  (defgeneric version-pre-release-p (version)
    (:documentation "Returns non-NIL if VERSION is a pre-release.")
    (:method ((version string))
      (version-pre-release-p (make-version version))))
  (defgeneric version-pre-release-for (version)
    (:documentation "If VERSION is a pre-release, returns an version specifier
stating for which version it is a pre-release.")
    (:method ((version string))
      (version-pre-release-for (make-version version))))
  (when (and (fboundp 'version<)
             (not (typep (symbol-function 'version<) 'generic-function)))
    ;; ASDF 3.4: Turned from function to generic function. Only conditionally
    ;; making it unbound because this is an extension point for the user and we
    ;; don't want to gratuitously remove their methods.
    (fmakunbound 'version<))
  (defgeneric version< (version1 version2)
    (:documentation "Returns non-NIL if VERSION2 is strictly newer than VERSION1.")
    (:method ((version1 string) version2)
      (version< (make-version version1) version2))
    (:method (version1 (version2 string))
      (version< version1 (make-version version2))))
  (defgeneric version-string (version)
    (:documentation "Return a string that represents VERSION.")
    (:method ((version string))
      ;; Round trip instead of returning VERSION, in case VERSION is invalid.
      (version-string (make-version version))))

  (defun version<= (version1 version2)
    "Given two version strings, return T if the second is newer or the same"
    (not (version< version2 version1)))
  (defun version= (version1 version2)
    "Given two version strings, return T if the first is newer or the same and
the second is also newer or the same."
    (and (version<= version1 version2)
         (version<= version2 version1))))

;; Semantic version class
(with-upgradability ()
  (defparameter *semver-valid-chars*
    '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\-)
    "List of all characters valid in pre-release and build metadata segments.")

  (defclass semantic-version (version-object)
    (core-segment
     pre-release-segment
     build-metadata-segment)
    (:documentation
     "Implements semantic version parsing and ordering as specified by
     https://semver.org/spec/v2.0.0.html"))

  (defun split-semver-string (version-string)
    "Given a semver string, split it into its three basic components: the core
segment, the pre-release segment, and the build segment. Returns the three
components in a list. If the component is missing from the string, it returns
NIL."
    (let ((pre-release-separator-position (position #\- version-string))
          (build-separator-position (position #\+ version-string)))
      ;; If the pre-release and build separators both exist and the former comes
      ;; after the latter, treat the former as not existing.
      (when (and pre-release-separator-position
                 build-separator-position
                 (> pre-release-separator-position build-separator-position))
        (setf pre-release-separator-position nil))
      (list
       ;; The core segment
       (subseq version-string 0 (or pre-release-separator-position
                                    build-separator-position))
       ;; The pre-release segment
       (when pre-release-separator-position
         (subseq version-string (1+ pre-release-separator-position)
                 build-separator-position))
       ;; The build segment
       (when build-separator-position
         (subseq version-string (1+ build-separator-position))))))

  (defun parse-semver-dot-separated-segment (segment-string identifier-valid-test transform on-error)
    "Given a string consisting of any number of identifiers separated by
#\\. characters, return a list of the identifiers.

Each identifier is checked with IDENTIFIER-VALID-TEST to ensure it is valid.

Then if the identifier is valid, TRANSFORM is called to canonicalize the
identifier.

If there are any invalid segments, ON-ERROR is called with a string describing
the problem."
    (let ((raw-identifiers (split-string segment-string :separator '(#\.))))
      (dolist (raw-identifier raw-identifiers)
        (when (emptyp raw-identifier)
          (call-function on-error (format nil "Invalid identifier ~S" raw-identifier)))
        (unless (call-function identifier-valid-test raw-identifier)
          (call-function on-error (format nil "Invalid identifier ~S" raw-identifier))))
      (mapcar transform raw-identifiers)))

  (defun valid-core-segment-identifier-p (string &key (on-leading-zero :warn))
    "An identifier is valid for use in the core segment if it is a non-negative
integer.

ON-LEADING-ZERO specifies how to handle leading zeros in identifier. If :WARN,
a warning is signaled and the identifier is considered valid. If :IGNORE, the
identifier is considered valid. If INVALID, the identifier is not considered
valid."
    (check-type on-leading-zero (member :warn :invalid :ignore))
    (and (every #'digit-char-p string)
         (or (= 1 (length string))
             (not (eql (aref string 0) #\0))
             (ecase on-leading-zero
               (:warn
                (warn "Identifier ~S contains a leading zero." string)
                t)
               (:ignore
                t)
               (:invalid
                nil)))))

  (defun valid-pre-release-segment-identifier-p (string &key (on-leading-zero :warn))
    "An identifier is valid for use in the pre-release segment if it is a
non-negative integer or consists entirely of alphanumeric and #\\- characters.

See VALID-CORE-SEGMENT-IDENTIFIER-P for a discussion of ON-LEADING-ZERO."
    (check-type on-leading-zero (member :warn :invalid :ignore))
    (and (every (lambda (c) (member c *semver-valid-chars*)) string)
         (or (notevery #'digit-char-p string)
             (valid-core-segment-identifier-p string :on-leading-zero on-leading-zero))))

  (defun valid-build-segment-identifier-p (string)
    "An identifier is valid for use in the build segment if it consists
entirely of alphanumeris and #\\- characters."
    (every (lambda (c) (member c *semver-valid-chars*)) string))

  (defun transform-pre-release-segment-identifier (string)
    "If STRING consistes only of numeric characters, parse it as an integer and
return it. Otherwise, return STRING."
    (let ((as-integer (ignore-errors (parse-integer string))))
      (if (and as-integer (not (minusp as-integer)))
          as-integer
          string)))

  (defun parse-semver-string (version-string)
    "Given a string nominally representing a version string in the semantic
version grammar, return three VALUES: the list of core segment identifiers, the
list of pre-release segment identifiers, and the list of build metadata
identifiers.

If VERSION-STRING contains leading zeros in a numeric identifier, a warning is
signaled.

If VERSION-STRING is otherwise invalid, a VERSION-STRING-INVALID-ERROR is signaled."
    (destructuring-bind (core-segment-string pre-release-segment-string build-segment-string)
        (split-semver-string version-string)
      (flet ((invalid (&optional reason)
               (error 'version-string-invalid-error :version-string version-string
                                                    :reason reason)))
        (when (null core-segment-string)
          (invalid "There is no core segment."))
        (when (and (stringp pre-release-segment-string)
                   (emptyp pre-release-segment-string))
          (invalid "There is a #\\- character, but the pre-release segment is empty."))
        (when (and (stringp build-segment-string)
                   (emptyp build-segment-string))
          (invalid "There is a #\\+ character, but the build segment is empty."))

        (let ((core-segment
                (parse-semver-dot-separated-segment core-segment-string 'valid-core-segment-identifier-p #'parse-integer #'invalid))
              (pre-release-segment
                (parse-semver-dot-separated-segment pre-release-segment-string 'valid-pre-release-segment-identifier-p 'transform-pre-release-segment-identifier #'invalid))
              (build-metadata
                (parse-semver-dot-separated-segment build-segment-string 'valid-build-segment-identifier-p #'identity #'invalid)))
          (values core-segment pre-release-segment build-metadata)))))

  (defmethod initialize-instance :after ((version semantic-version) &key version-string)
    (with-slots (core-segment pre-release-segment build-metadata-segment) version
      (multiple-value-setq
          (core-segment pre-release-segment build-metadata-segment)
        (parse-semver-string version-string))))

  (defun semver-pre-release-segment-< (identifier1 identifier2)
    (cond
      ((and (integerp identifier1) (integerp identifier2))
       (< identifier1 identifier2))
      ((integerp identifier1)
       t)
      ((integerp identifier2)
       nil)
      (t
       (string< identifier1 identifier2))))
  (defun semver-pre-release-< (pre-release-segment-1 pre-release-segment-2)
    (or (and pre-release-segment-1 (null pre-release-segment-2))
        (lexicographic< 'semver-pre-release-segment-< pre-release-segment-1 pre-release-segment-2)))

  (defmethod version-next ((version semantic-version))
    (with-slots (core-segment) version
      (let ((new-core-segment (copy-list core-segment)))
        (incf (car (last new-core-segment)))
        (make-version (format nil "~{~D~^.~}" new-core-segment) :class (class-of version)))))
  (defmethod version-pre-release-p ((version semantic-version))
    (with-slots (pre-release-segment) version
      (not (null pre-release-segment))))
  (defmethod version-pre-release-for ((version semantic-version))
    (with-slots (core-segment) version
      (make-version (format nil "~{~D~^.~}" core-segment) :class (class-of version))))
  (defmethod version< ((version1 semantic-version) (version2 semantic-version))
    (with-slots ((core-segment-1 core-segment) (pre-release-segment-1 pre-release-segment)) version1
      (with-slots ((core-segment-2 core-segment) (pre-release-segment-2 pre-release-segment)) version2
        (or (lexicographic< #'< core-segment-1 core-segment-2)
            (and (equal core-segment-1 core-segment-2)
                 (semver-pre-release-< pre-release-segment-1 pre-release-segment-2))))))
  (defmethod version-string ((version semantic-version))
    (with-slots (core-segment pre-release-segment build-metadata-segment) version
      (format nil "~{~D~^.~}~@[-~{~A~^.~}~]~@[+~{~A~^.~}~]"
              core-segment pre-release-segment build-metadata-segment))))

;; ASDF's (slightly) opinionated default version.
(with-upgradability ()
  (defclass default-version (semantic-version)
    ()
    (:documentation
     "A version specifier that parses and orders identically to
SEMANTIC-VERSION. However, no build metadata is allowed and the pre-release
segment can consist of at most two identifiers. The first must be \"alpha\",
\"beta\", or \"rc\". The second must be an integer."))

  (defmethod initialize-instance :after ((version default-version) &key version-string)
    (with-slots (pre-release-segment build-metadata-segment) version
      (flet ((invalid (&optional reason)
               (error 'version-string-invalid-error :version-string version-string
                                                    :reason reason)))
        (unless (null build-metadata-segment)
          (invalid "The build metadata segment must not exist."))
        (unless (or (null pre-release-segment)
                    (and (= 1 (length pre-release-segment))
                         (or (equal "alpha" (first pre-release-segment))
                             (equal "beta" (first pre-release-segment))
                             (equal "rc" (first pre-release-segment))))
                    (and (= 2 (length pre-release-segment))
                         (or (equal "alpha" (first pre-release-segment))
                             (equal "beta" (first pre-release-segment))
                             (equal "rc" (first pre-release-segment)))
                         (integerp (second pre-release-segment))))
          (invalid "The pre-release segment must be absent or consist of \"alpha\", \"beta\", or \"rc\", optionally followed by an integer."))))))

(with-upgradability ()
  (defun simple-version-constraint-p (constraint)
    (and (listp constraint)
         (and (= (length constraint) 2))
         (member (first constraint) '(:>= :> :<= :< := :/=))
         (or (stringp (second constraint))
             (typep (second constraint) 'version-object))))
  (defun simple-version-constraint-satisfied-p (operator version requested
                                                &key (strip-pre-release-p (not (version-pre-release-p requested))))
    (let ((version (if (and strip-pre-release-p (version-pre-release-p version))
                       (version-pre-release-for version)
                       version)))
      (ecase operator
        (:>=
         (not (version< version requested)))
        (:>
         (version< requested version))
        (:<=
         (not (version< requested version)))
        (:<
         (version< version requested))
        (:=
         (version= version requested))
        (:/=
         (not (version= version requested))))))

  (defun compound-version-constraint-p (constraint)
    (and (listp constraint)
         (member (first constraint) '(:and :or))))
  (defun compound-version-constraint-satisfied-p (operator version subcs
                                                  &rest args
                                                  &key strip-pre-release-p)
    (declare (ignore strip-pre-release-p))
    (ecase operator
      (:and
       (every (lambda (subc) (apply #'version-constraint-satisfied-p version subc args))
              subcs))
      (:or
       (some (lambda (subc) (apply #'version-constraint-satisfied-p version subc args))
             subcs))))

  (defun compatible-version-constraint-p (constraint)
    (or (and (listp constraint)
             (and (= (length constraint) 2))
             (eql (first constraint) :compatible)
             (or (stringp (second constraint))
                 (typep (second constraint) 'version-object)))
        (stringp constraint)
        (typep constraint 'version-object)))
  (defun compatible-version-constraint-satisfied-p (version requested
                                                    &rest args
                                                    &key compatible-versions
                                                      (strip-pre-release-p (not (version-pre-release-p requested))))
    (let ((version (if (and strip-pre-release-p (version-pre-release-p version))
                       (version-pre-release-for version)
                       version)))
      (and (not (version< version requested))
           (or (null compatible-versions)
               (apply #'version-constraint-satisfied-p requested compatible-versions
                      (remove-plist-key :compatible-versions args))))))

  (defun version-constraint-satisfied-p (version constraint &rest args &key strip-pre-release-p)
    (declare (ignore strip-pre-release-p))
    (let ((version (ensure-version version)))
      (cond
        ((simple-version-constraint-p constraint)
         (apply #'simple-version-constraint-satisfied-p
                (first constraint) version
                (ensure-version (second constraint)
                                :class (class-of version))
                args))
        ((compatible-version-constraint-p constraint)
         (let ((requested (ensure-version (if (listp constraint) (second constraint) constraint)
                                          :class (class-of version))))
           (apply #'compatible-version-constraint-satisfied-p version requested args)))
        ((compound-version-constraint-p constraint)
         (apply #'compound-version-constraint-satisfied-p
                (first constraint) version (rest constraint)
                args))
        (t t)))))

(with-upgradability ()
  (define-condition deprecated-function-condition (condition)
    ((name :initarg :name :reader deprecated-function-name)))
  (define-condition deprecated-function-style-warning (deprecated-function-condition style-warning) ())
  (define-condition deprecated-function-warning (deprecated-function-condition warning) ())
  (define-condition deprecated-function-error (deprecated-function-condition error) ())
  (define-condition deprecated-function-should-be-deleted (deprecated-function-condition error) ())

  (defun deprecated-function-condition-kind (type)
    (ecase type
      ((deprecated-function-style-warning) :style-warning)
      ((deprecated-function-warning) :warning)
      ((deprecated-function-error) :error)
      ((deprecated-function-should-be-deleted) :delete)))

  (defmethod print-object ((c deprecated-function-condition) stream)
    (let ((name (deprecated-function-name c)))
      (cond
        (*print-readably*
         (let ((fmt "#.(make-condition '~S :name ~S)")
               (args (list (type-of c) name)))
           (if *read-eval*
               (apply 'format stream fmt args)
               (error "Can't print ~?" fmt args))))
        (*print-escape*
         (print-unreadable-object (c stream :type t) (format stream ":name ~S" name)))
        (t
         (let ((*package* (find-package :cl))
               (type (type-of c)))
           (format stream
                   (if (eq type 'deprecated-function-should-be-deleted)
                       "~A: Still defining deprecated function~:P ~{~S~^ ~} that promised to delete"
                       "~A: Using deprecated function ~S -- please update your code to use a newer API.~
~@[~%The docstring for this function says:~%~A~%~]")
                   type name (when (symbolp name) (documentation name 'function))))))))

  (defun notify-deprecated-function (status name)
    (ecase status
      ((nil) nil)
      ((:style-warning) (style-warn 'deprecated-function-style-warning :name name))
      ((:warning) (warn 'deprecated-function-warning :name name))
      ((:error) (cerror "USE FUNCTION ANYWAY" 'deprecated-function-error :name name))))

  (defun version-deprecation (version &key (style-warning nil)
                                        (warning (version-next style-warning))
                                        (error (version-next warning))
                                        (delete (version-next error)))
    "Given a VERSION string, and the starting versions for notifying the programmer of
various levels of deprecation, return the current level of deprecation as per WITH-DEPRECATION
that is the highest level that has a declared version older than the specified version.
Each start version for a level of deprecation can be specified by a keyword argument, or
if left unspecified, will be the VERSION-NEXT of the immediate lower level of deprecation."
    (let ((version (if (version-pre-release-p version)
                       (version-pre-release-for version)
                       version)))
      (cond
        ((and delete (version<= delete version)) :delete)
        ((and error (version<= error version)) :error)
        ((and warning (version<= warning version)) :warning)
        ((and style-warning (version<= style-warning version)) :style-warning))))

  (defmacro with-deprecation ((level) &body definitions)
    "Given a deprecation LEVEL (a form to be EVAL'ed at macro-expansion time), instrument the
DEFUN and DEFMETHOD forms in DEFINITIONS to notify the programmer of the deprecation of the function
when it is compiled or called.

Increasing levels (as result from evaluating LEVEL) are: NIL (not deprecated yet),
:STYLE-WARNING (a style warning is issued when used), :WARNING (a full warning is issued when used),
:ERROR (a continuable error instead), and :DELETE (it's an error if the code is still there while
at that level).

Forms other than DEFUN and DEFMETHOD are not instrumented, and you can protect a DEFUN or DEFMETHOD
from instrumentation by enclosing it in a PROGN."
    (let ((level (eval level)))
      (check-type level (member nil :style-warning :warning :error :delete))
      (when (eq level :delete)
        (error 'deprecated-function-should-be-deleted :name
               (mapcar 'second
                       (remove-if-not #'(lambda (x) (member x '(defun defmethod)))
                                      definitions :key 'first))))
      (labels ((instrument (name head body whole)
                 (if level
                     (let ((notifiedp
                            (intern (format nil "*~A-~A-~A-~A*"
                                            :deprecated-function level name :notified-p))))
                       (multiple-value-bind (remaining-forms declarations doc-string)
                           (parse-body body :documentation t :whole whole)
                         `(progn
                            (defparameter ,notifiedp nil)
                            ;; tell some implementations to use the compiler-macro
                            (declaim (inline ,name))
                            (define-compiler-macro ,name (&whole form &rest args)
                              (declare (ignore args))
                              (notify-deprecated-function ,level ',name)
                              form)
                            (,@head ,@(when doc-string (list doc-string)) ,@declarations
                                    (unless ,notifiedp
                                      (setf ,notifiedp t)
                                      (notify-deprecated-function ,level ',name))
                                    ,@remaining-forms))))
                     `(progn
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (setf (compiler-macro-function ',name) nil))
                        (declaim (notinline ,name))
                        (,@head ,@body)))))
        `(progn
           ,@(loop :for form :in definitions :collect
               (cond
                 ((and (consp form) (eq (car form) 'defun))
                  (instrument (second form) (subseq form 0 3) (subseq form 3) form))
                 ((and (consp form) (eq (car form) 'defmethod))
                  (let ((body-start (if (listp (third form)) 3 4)))
                    (instrument (second form)
                                (subseq form 0 body-start)
                                (subseq form body-start)
                                form)))
                 (t
                  form))))))))

;; All of these were deprecated in 3.4
(with-upgradability ()
  ;; These implement the actual logic of the deprecated functions. Doing it
  ;; this way ensures that we don't get style warnings when loading UIOP
  ;; itself.
  (defun %unparse-version (version-list)
    (format nil "~{~D~^.~}" version-list))
  (defun %parse-version (version-string &optional on-error)
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (%unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))
  (defun %next-version (version)
    (when version
      (let ((version-list (%parse-version version)))
        (incf (car (last version-list)))
        (%unparse-version version-list))))

  (with-deprecation ((version-deprecation *uiop-version* :style-warning "3.4" :delete "4.0"))
    (defun unparse-version (version-list)
      "DEPRECATED. Use VERSION-STRING instead.

From a parsed version (a list of natural numbers), compute the version string"
      (%unparse-version version-list))

    (defun parse-version (version-string &optional on-error)
      "DEPRECATED. Use MAKE-VERSION instead.

Parse a VERSION-STRING as a series of natural numbers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
      (%parse-version version-string on-error))

    (defun next-version (version)
      "DEPRECATED. Use VERSION-NEXT instead.

When VERSION is not nil, it is a string, then parse it as a version, compute the next version
and return it as a string."
      (%next-version version))))
