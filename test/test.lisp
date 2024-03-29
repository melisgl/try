(in-package :try-test)

(deftest test-all ()
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-case* :upcase)
        (*print-indentation* 2)
        (*print-duration* nil)
        (*print-compactly* nil)
        (*defer-describe* nil))
    (test-utils)
    (test-is)
    (test-checks)
    (test-floats)
    (test-trial)
    (test-try)))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :try-test))
    (print (try 'test-all :debug debug :print print :describe describe))))

#+nil
(time (test))

#+nil
(test-all)
