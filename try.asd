;;;; -*- mode: Lisp -*-

;;; See TRY::@TRY-MANUAL for the user guide.
(asdf:defsystem :try
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/try"
  :bug-tracker "https://github.com/melisgl/try/issues"
  :source-control (:git "https://github.com/melisgl/try.git")
  :description "Try is an extensible test framework with equal support
  for interactive and non-interactive workflows."
  :long-description "Try stays as close to normal Lisp evaulation
  rules as possible. Tests are functions that record the checks they
  perform as events. These events provide the means of customization
  of what to debug, print, rerun. There is a single fundamental check,
  the extensible IS macro. Everything else is built on top."
  :defsystem-depends-on (#:try.asdf)
  :depends-on (#:alexandria #:cl-ppcre #:closer-mop #:ieee-floats #:mgl-pax
               #:trivial-gray-streams #:uiop)
  :pathname "src/"
  :serial t
  ;; We compile each file in a (WITH-COMPILATION-UNIT (:OVERRIDE T)
  ;; ...) so that they are treated as separate compilation units (even
  ;; though they are all nested in another WITH-COMPILATION-UNIT) to
  ;; get warnings about forward references from one file to a later
  ;; one.
  :around-compile "try/asdf:compile-wrapper"
  :components ((:file "package")
               (:file "util")
               (:file "gray-stream")
               (:file "early")
               (:file "debug")
               (:file "events")
               (:file "outcome")
               (:file "result")
               (:file "is")
               (:file "checks")
               (:file "floats")
               (:file "trial-event")
               (:file "error")
               (:file "count")
               (:file "trial")
               (:file "test")
               (:file "testable")
               (:file "rerun")
               (:file "print")
               (:file "collect")
               (:file "try")
               (:file "replay")
               (:file "manual")
               (:file "doc"))
  :in-order-to ((asdf:test-op (asdf:test-op "try/test"))))

(asdf:defsystem :try/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Test system for TRY."
  :depends-on (#:try)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "test-util")
                             (:file "test-is")
                             (:file "test-checks")
                             (:file "test-floats")
                             (:file "test-trial")
                             (:file "test-try")
                             (:file "test-rerun")
                             (:file "test-count")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:try-test '#:test)))
