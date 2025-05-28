(in-package :try)

(defvar *trial* nil)
(defvar *try-id* nil)
(defvar *record-event* nil)

(declaim (special *print-backtrace*))

(define-glossary-term @tryvar (:title "Try var")
  "There are lots of special variables that affect TRY. To avoid the
  plight of Common Lisp STREAMs and guarantee consistent output and
  behaviour even if these variables are changed during a single TRY
  run, the values of variables are captured when TRY is first invoked.
  That is, when the outermost test function is entered. These
  variables are called Try vars.")

;;; For TRY-TEST::TEST-TRY/VARS
(defvar *try-vars* ())

(defmacro define-try-var (var value &optional doc)
  `(locally
       (defvar ,var ,value ,(format nil "@TRYVAR.~@[ ~A~]" doc))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew ',var *try-vars*))))
