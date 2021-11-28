(uiop/package:define-package :uiop/version-specifier
  (:recycle :uiop/version-specifier :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   ;; Basic version API
   #:version #:version-string-invalid-error #:version-pre-release-p #:version-pre-release-for
   #:version< #:version-string

   ;; Semantic versions
   #:semantic-version

   ;; Default versions
   #:default-version))
(in-package :uiop/version-specifier)

;; Base version class
(with-upgradability ()
  (defclass version ()
    ()
    (:documentation
     "The base class of version specifiers."))

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

  (defgeneric version-pre-release-p (version)
    (:documentation "Returns non-NIL if VERSION is a pre-release."))
  (defgeneric version-pre-release-for (version)
    (:documentation "If VERSION is a pre-release, returns an version specifier
stating for which version it is a pre-release."))
  (defgeneric version< (version-1 version-2)
    (:documentation "Returns non-NIL if VERSION-1 is ordered before VERSION-2."))
  (defgeneric version-string (version)
    (:documentation "Return a string that represents VERSION.")))

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

  (defclass semantic-version (version)
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

  (defmethod version-pre-release-p ((version semantic-version))
    (with-slots (pre-release-segment) version
      (not (null pre-release-segment))))
  (defmethod version-pre-release-for ((version semantic-version))
    (with-slots (core-segment) version
      (make-instance (class-of version) :version-string (format nil "~{~D~^.~}" core-segment))))
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

;; ASDF's opinionated default version.
(with-upgradability ()
  (defclass default-version (semantic-version)
    ()
    (:documentation
     "A version specifier that parses and orders identically to
SEMANTIC-VERSION. However, no build metadata is allowed and the pre-release
segment can consists of at most two identifiers. The first must be alpha, beta,
or rc. The second must be an integer."))

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
