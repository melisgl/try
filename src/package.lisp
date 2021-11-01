(mgl-pax:define-package #:try
  (:use #:common-lisp #:mgl-pax #:named-readtables #:pythonic-string-reader)
  (:shadow #:restart-case
           #:restart-bind))
