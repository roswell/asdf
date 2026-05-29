(defpackage :test-package-2 (:use cl))
(in-package :test-package-2)

(defmacro package-dependent-code ()
  (if (find-package :test-package)
      '(progn
        (import (intern (symbol-name '#:*file1*) :test-package)
                :test-package-2)
        (assert (asdf-test:asymval '#:*file1* :test-package-2)))
      '(assert (not (uiop:find-symbol* '#:*file1* :test-package-2
                                      nil)))))

(package-dependent-code)
