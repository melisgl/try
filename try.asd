;;;; -*- mode: Lisp -*-

;;; See TRY:@TRY-MANUAL for the user guide.
(asdf:defsystem :try
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/try"
  :bug-tracker "https://github.com/melisgl/try/issues"
  :source-control (:git "https://github.com/melisgl/try.git")
  :description "Try is a test framework."
  :long-description "Try is what we get if we make tests functions and
  build a test framework on top of the condition system like
  [Stefil](https://common-lisp.net/project/stefil/index-old.shtml)
  did, but also address the issue of rerunning and replaying, make the
  IS check more capable, use the types of the condition hierarchy to
  parameterize what to debug, print, rerun, and document the whole
  thing."
  :defsystem-depends-on (#:try.asdf)
  :depends-on (#:alexandria #:closer-mop #:ieee-floats #:mgl-pax
                            #:trivial-gray-streams #:uiop)
  :pathname "src/"
  :serial t
  :components
  ;; By compiling each file in a (WITH-COMPILATION-UNIT (:OVERRIDE T)
  ;; ...), they are treated as separate compilation units (even though
  ;; they are all nested in another WITH-COMPILATION-UNIT), and we get
  ;; warnings about forward references from one file to a later one.
  ((:file "package" :around-compile "try/asdf:compile-wrapper")
   (:file "util" :around-compile "try/asdf:compile-wrapper")
   (:file "gray-stream" :around-compile "try/asdf:compile-wrapper")
   (:file "early" :around-compile "try/asdf:compile-wrapper")
   (:file "debug" :around-compile "try/asdf:compile-wrapper")
   (:file "events" :around-compile "try/asdf:compile-wrapper")
   (:file "outcome" :around-compile "try/asdf:compile-wrapper")
   (:file "result" :around-compile "try/asdf:compile-wrapper")
   (:file "is" :around-compile "try/asdf:compile-wrapper")
   (:file "checks" :around-compile "try/asdf:compile-wrapper")
   (:file "floats" :around-compile "try/asdf:compile-wrapper")
   (:file "trial-event" :around-compile "try/asdf:compile-wrapper")
   (:file "error" :around-compile "try/asdf:compile-wrapper")
   (:file "count" :around-compile "try/asdf:compile-wrapper")
   (:file "trial" :around-compile "try/asdf:compile-wrapper")
   (:file "test" :around-compile "try/asdf:compile-wrapper")
   (:file "testable" :around-compile "try/asdf:compile-wrapper")
   (:file "rerun" :around-compile "try/asdf:compile-wrapper")
   (:file "print" :around-compile "try/asdf:compile-wrapper")
   (:file "collect" :around-compile "try/asdf:compile-wrapper")
   (:file "try" :around-compile "try/asdf:compile-wrapper")
   (:file "replay" :around-compile "try/asdf:compile-wrapper")
   (:file "manual" :around-compile "try/asdf:compile-wrapper")
   (:file "doc" :around-compile "try/asdf:compile-wrapper"))
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
