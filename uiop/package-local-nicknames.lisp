;;; -------------------------------------------------------------------------
;;; Export the package-local-nicknames API. This cannot be done from
;;; uiop/package due to different ways implementations handle package variance.

(uiop/package:define-package :uiop/package-local-nicknames
    (:use :common-lisp)
  ;; Hack to make sure uiop/package is loaded first (to portably get
  ;; :PACKAGE-LOCAL-NICKNAMES onto *FEATURES*)
  (:import-from :uiop/package)
  #+package-local-nicknames
  (:import-from #+allegro #:excl
                #+sbcl #:sb-ext
                #+(or clasp abcl ecl) #:ext
                #+ccl #:ccl
                #+lispworks #:hcl
                #-(or allegro sbcl clasp abcl ccl lispworks ecl)
                (error "Don't know from which package this lisp supplies the local-package-nicknames API.")
                #:remove-package-local-nickname #:package-local-nicknames #:add-package-local-nickname)
  #+package-local-nicknames
  (:export #:add-package-local-nickname #:remove-package-local-nickname #:package-local-nicknames))

(in-package :uiop/package-local-nicknames)
