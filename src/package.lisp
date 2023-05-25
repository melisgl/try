(mgl-pax:define-package #:try
  (:documentation "See TRY::@TRY-MANUAL.")
  (:use #:common-lisp #:mgl-pax #:named-readtables #:pythonic-string-reader)
  (:shadow #:restart-case
           #:restart-bind))
