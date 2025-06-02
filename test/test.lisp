(in-package :try-test)

(deftest test-all ()
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-case* :upcase)
        (*print-indentation* 2)
        (*print-duration* nil)
        (*print-compactly* nil)
        (*print-backtrace* t)
        (*defer-describe* nil)
        (*rerun-context* nil))
    (test-utils)
    (test-is)
    (test-checks)
    (test-floats)
    (test-trial)
    (test-try)
    (test-transcripts)))

(deftest test-transcripts ()
  #+sbcl
  (signals-not (pax:transcription-error)
    (with-new-implicit-try
      (pax:document try::@try-manual :format nil))))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (let ((try::*allow-nested-try* t))
    (warn-on-tests-not-run ((find-package :try-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(time (test))

#+nil
(test-all)
