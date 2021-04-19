(uiop/package:define-package :uiop/version
  (:recycle :uiop/version :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:*uiop-version*
   #:parse-version #:unparse-version #:version< #:version<= #:version= ;; version support, moved from uiop/utility
   #:next-version
   #:deprecated-function-condition #:deprecated-function-name ;; deprecation control
   #:deprecated-function-style-warning #:deprecated-function-warning
   #:deprecated-function-error #:deprecated-function-should-be-deleted
   #:version-deprecation #:with-deprecation))
(in-package :uiop/version)

(with-upgradability ()
  (defparameter *uiop-version* "3.3.5.7")

  (defparameter *pre-release-and-build-metadata-chars*
    '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\-))

  (defun unparse-version (version-list &optional pre-release-list build-metadata)
    "From a parsed version (a list of natural numbers), compute the version
string. VERSION-LIST is a list of natural numbers. PRE-RELEASE-LIST is a list
of pre-release identifiers (strings or natural numbers). BUILD-METADATA is a
list of build metadata identifiers (strings or )string or NIL."
    (format nil "~{~D~^.~}~@[-~{~A~^.~}~]~@[+~{~A~^.~}~]" version-list pre-release-list build-metadata))

  (defun split-version-string (version-string)
    "Given a semver string, split it into its three basic components: the core
segment, the pre-release segment, and the build segment. Returns the three
components in a list. If the component is missing from the string, it returns
nil."
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

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a core version followed by an optional
pre-release segment (preceded by a -), followed by an optional build metadata
segment (preceded by a +). Each segment is returned as a separate VALUE.

The core version segment must consist entirely of natural numbers separated by
dots. The pre-release segment consists of a series of identifiers (consisting
of the characters 0-9, a-z, A-Z, and -), separated by dots. The build metadata
segment consists of a series of identifiers (consisting of the characters 0-9,
a-z, A-Z, and -), separated by dots.

This grammar is heavily inspired by semver v2.0's grammar with the major
difference that the core version segment is not limited to exactly three dot
separated natural numbers.

When invalid, ON-ERROR is called as per CALL-FUNCTION before returning NIL,
with format arguments explaining why the version is invalid.  ON-ERROR is also
called if the version is not canonical in that it doesn't print back to itself,
but the values are returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (destructuring-bind (core-segment-string pre-release-segment-string build-segment-string)
          (split-version-string version-string)
        (labels ((invalid ()
                   (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                                  'parse-version version-string))
                 (parse-dot-separated-segment (segment-string character-test transform)
                   (unless (loop :for prev = nil :then c :for c :across segment-string
                                 :always (or (funcall character-test c)
                                             (and (eql c #\.) prev (not (eql prev #\.))))
                                 :finally (return (and c (funcall character-test c))))
                     (invalid)
                     (return))
                   (mapcar transform (split-string segment-string :separator ".")))
                 (parse-core-segment (segment-string)
                   (parse-dot-separated-segment segment-string #'digit-char-p #'parse-integer))
                 (parse-pre-release-segment (segment-string)
                   (parse-dot-separated-segment segment-string
                                                (lambda (c)
                                                  (member c *pre-release-and-build-metadata-chars*))
                                                (lambda (x)
                                                  (let ((as-integer (ignore-errors (parse-integer x))))
                                                    (if (and as-integer (not (minusp as-integer)))
                                                        as-integer
                                                        x)))))
                 (parse-build-segment (segment-string)
                   (parse-dot-separated-segment segment-string
                                                (lambda (c)
                                                  (member c *pre-release-and-build-metadata-chars*))
                                                #'identity)))
          (unless core-segment-string
            (invalid)
            (return))
          (let* ((core-segment (parse-core-segment core-segment-string))
                 (pre-release-segment (when pre-release-segment-string
                                        (parse-pre-release-segment pre-release-segment-string)))
                 (build-segment (when build-segment-string
                                  (parse-build-segment build-segment-string)))
                 (normalized-version (unparse-version core-segment pre-release-segment build-segment)))
            (unless (equal version-string normalized-version)
              (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
            (values core-segment pre-release-segment build-segment))))))

  (defun next-version (version)
    "When VERSION is not nil, it is a string, then parse it as a version, compute the next version
and return it as a string."
    (when version
      (let ((version-list (parse-version version)))
        (incf (car (last version-list)))
        (unparse-version version-list))))

  (defun version< (version1 version2)
    "Given two version strings, return T if the second is strictly newer"
    (multiple-value-bind (core-1 pre-release-1)
        (parse-version version1)
      (multiple-value-bind (core-2 pre-release-2)
          (parse-version version2)
        (labels ((identifier< (id1 id2)
                   (cond
                     ((and (integerp id1) (integerp id2))
                      (< id1 id2))
                     ((integerp id1)
                      t)
                     ((integerp id2)
                      nil)
                     (t
                      (string< id1 id2))))
                 (pre-release-< (pr1 pr2)
                   (or
                    (and pr1 (null pr2))
                    (lexicographic< #'identifier< pr1 pr2))))
          (or (lexicographic< '< core-1 core-2)
              (and (equal core-1 core-2)
                   (pre-release-< pre-release-1 pre-release-2)))))))

  (defun version<= (version1 version2)
    "Given two version strings, return T if the second is newer or the same"
    (not (version< version2 version1))))

  (defun version= (version1 version2)
    "Given two version strings, return T if the first is newer or the same and
the second is also newer or the same."
    (and (version<= version1 version2)
         (version<= version2 version1)))


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
                                        (warning (next-version style-warning))
                                        (error (next-version warning))
                                        (delete (next-version error)))
    "Given a VERSION string, and the starting versions for notifying the programmer of
various levels of deprecation, return the current level of deprecation as per WITH-DEPRECATION
that is the highest level that has a declared version older than the specified version.
Each start version for a level of deprecation can be specified by a keyword argument, or
if left unspecified, will be the NEXT-VERSION of the immediate lower level of deprecation."
    (cond
      ((and delete (version<= delete version)) :delete)
      ((and error (version<= error version)) :error)
      ((and warning (version<= warning version)) :warning)
      ((and style-warning (version<= style-warning version)) :style-warning)))

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
