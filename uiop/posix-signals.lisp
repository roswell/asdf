;;;; -------------------------------------------------------------------------
;;;; posix-signals - This package defines the standard POSIX signals.
;;;
;;;
;;; Currently it only defines signal codes that are defined the same way in all platforms and systems.
;;;
;;; All the codes were defined by using as reference:
;;;  - https://man7.org/linux/man-pages/man7/signal.7.html
;;;  - https://www.freebsd.org/cgi/man.cgi?query=signal&apropos=0&sektion=0&manpath=FreeBSD+13.0-RELEASE+and+Ports&arch=default&format=html

(uiop/package:define-package :uiop/posix-signals
  (:use :uiop/common-lisp :uiop/utility)
  (:export #:+SIGHUP+ #:+SIGINT+ #:+SIGQUIT+ #:+SIGILL+ #:+SIGTRAP+
  #:+SIGABRT+ #:+SIGIOT+ #:+SIGFPE+ #:+SIGKILL+ #:+SIGSEGV+
  #:+SIGPIPE+ #:+SIGALRM+ #:+SIGTERM+))
(in-package :uiop/posix-signals)

(with-upgradability ()
  (defconstant +SIGHUP+  1)
  (defconstant +SIGINT+  2)
  (defconstant +SIGQUIT+ 3)
  (defconstant +SIGILL+  4)
  (defconstant +SIGTRAP+ 5)
  (defconstant +SIGABRT+ 6)
  (defconstant +SIGIOT+  6)
  (defconstant +SIGFPE+  8)
  (defconstant +SIGKILL+ 9)
  (defconstant +SIGSEGV+ 11)
  (defconstant +SIGPIPE+ 13)
  (defconstant +SIGALRM+ 14)
  (defconstant +SIGTERM+ 15))
