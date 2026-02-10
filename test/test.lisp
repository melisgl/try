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
  (signals-not (pax:transcription-error :handler #'continue)
    (ensure-directories-exist "test/data/")
    (with-new-implicit-try ()
      (try::update-try-docs :output-dir "test/data/")))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "try" "README")
   (asdf:system-relative-pathname "try" "test/data/README"))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "try" "README.md")
   (asdf:system-relative-pathname "try" "test/data/README.md")))

(defun check-files-the-same (file1 file2)
  (is (equal (alexandria:read-file-into-string (% file1))
             (alexandria:read-file-into-string (% file2)))
      :capture nil))


(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (let ((try::*allow-nested-try* t))
    (warn-on-tests-not-run ((find-package :try-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(time (test))

#+nil
(test-all)
