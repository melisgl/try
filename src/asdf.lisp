(defpackage :try/asdf
  (:use :common-lisp)
  (:export #:compile-wrapper))

(in-package :try/asdf)

(defun compile-wrapper (continuation)
  (with-compilation-unit (:override t)
    (funcall continuation)))
