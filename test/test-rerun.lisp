(in-package :try-test)

(deftest test-try/rerun ()
  (test-try/rerun/list-function-designators)
  (test-try/rerun/call-non-root-deftest-trial)
  (test-try/rerun/skipping)
  (test-try/rerun/retry)
  (test-try/rerun/implicit))


(deftest test-try/rerun/list-function-designators ()
  (with-test ("rerun ()")
    (let ((trial (try () :print nil)))
      (is (equal (try::cform trial) '(try ())))
      (is (passedp (try trial :print nil)))
      (let ((trial (try trial :print nil)))
        (is (equal (try::cform trial) '(try ())))
        (is (passedp (try trial :print nil))))))
  (with-test ("rerun DEFTEST")
    (with-test ("success")
      (let ((trial (try '%simple-success :print nil)))
        (is (equal (try::cform trial) '(%simple-success)))
        (is (passedp trial))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) '(%simple-success)))
          (is (passedp trial))
          (is (typep (verdict trial) 'verdict-skip)))))
    (with-test ("failure")
      (let ((trial (try '%simple-failure :print nil)))
        (is (failedp trial))
        (is (equal (try::cform trial) '(%simple-failure)))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) '(%simple-failure)))
          (is (failedp trial))
          (is (not (typep (verdict trial) 'pass)))))))
  (with-test ("rerun FBOUNDP")
    (with-test ("success")
      (let ((trial (try '%is/success :print nil)))
        (is (equal (try::cform trial) '(try %is/success)))
        (is (passedp trial))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) '(try %is/success)))
          (is (passedp trial))
          (is (typep (verdict trial) 'verdict-skip)))))
    (with-test ("failure")
      (let ((trial (try '%is/failure :print nil)))
        (is (equal (try::cform trial) '(try %is/failure)))
        (is (failedp trial))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) '(try %is/failure)))
          (is (failedp trial))
          (is (not (typep (verdict trial) 'pass)))))))
  (with-test ("rerun not FBOUNDP")
    (signals (error :pred "not FBOUNDP")
      (try 'not-fboundp)))
  (with-test ("rerun FUNCTION")
    (with-test ("success")
      (let* ((fn (lambda () (is t)))
             (trial (try fn :print nil)))
        (is (equal (try::cform trial) `(try ,fn)))
        (is (passedp trial))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) `(try ,fn)))
          (is (passedp trial))
          (is (typep (verdict trial) 'verdict-skip)))))
    (with-test ("failure")
      (let* ((fn (lambda () (is nil)))
             (trial (try fn :print nil)))
        (is (equal (try::cform trial) `(try ,fn)))
        (is (failedp trial))
        (let ((trial (try trial :print nil)))
          (is (equal (try::cform trial) `(try ,fn)))
          (is (failedp trial))
          (is (not (typep (verdict trial) 'pass)))))))
  (with-test ("rerun LIST")
    (let* ((list '(%is/success %simple-success))
           (trial (try list :print nil)))
      (is (equal (try::cform trial) `(try ,list)))
      (is (passedp trial))
      (let ((trial (try trial :print nil)))
        (is (equal (try::cform trial) `(try ,list)))
        (is (passedp trial))
        (is (typep (verdict trial) 'verdict-skip))))
    (let* ((list '(%is/failure %simple-failure))
           (trial (try list :print nil)))
      (is (equal (try::cform trial) `(try ,list)))
      (is (failedp trial))
      (let ((trial (try trial :print nil)))
        (is (equal (try::cform trial) `(try ,list)))
        (is (failedp trial))
        (is (not (typep (verdict trial) 'pass))))))
  (with-test ("rerun list of trials")
    (let* ((trials (list (with-silent-implicit-try
                           (with-test (t0)
                             (is t)))
                         (with-silent-implicit-try
                           (with-test (t1)
                             (is t)))))
           (trial (try trials :rerun t :print nil)))
      (is (passedp trial))
      (is (try::try-trial-p trial))
      (is (equal (try::try-trial-testable trial) trials))))
  (with-test ("rerun PACKAGE")
    (%with-deftest-registry
      (%with-deftest ((test-name "try-test::%test-in-package") ())
        (let ((trial (try (find-package :try-test) :print nil)))
          (is (equal (try::cform trial) `(try ,(find-package :try-test))))
          (is (passedp trial))
          (let ((trial (try trial :print nil)))
            (is (equal (try::cform trial) `(try ,(find-package :try-test))))
            (is (passedp trial))
            (is (typep (verdict trial) 'verdict-skip)))))))
  (with-test ("rerun TRIAL")
    (with-test ("CALL-DEFTEST")
      (with-test ("not FBOUNDP DEFTEST")
        (%with-deftest ((test-name "try::%not-test-bound-p") ())
          (let ((trial (try test-name :print nil :collect t)))
            (fmakunbound test-name)
            (signals (error :pred "no longer FBOUNDP")
              (try trial)))))
      (with-test ("no longer TEST-BOUND-P")
        (%with-deftest ((test-name "try::%not-test-bound-p") ())
          (let ((trial (try `(,test-name) :print nil :collect t)))
            ;; Silence redifinition warnings.
            (handler-bind ((warning #'muffle-warning))
              (eval `(defun ,test-name ())))
            (signals (error :pred "no longer TRY:TEST-BOUND-P")
              (try (trial (first (children trial))))))))))
  (with-test ("not testable")
    (signals (error :pred "not testable")
      (try 7))))

(defun %is/success ()
  (is t))

(defun %is/failure ()
  (is nil))


(deftest test-try/rerun/call-non-root-deftest-trial ()
  (test-try/rerun/non-root-trial/with-test)
  (test-try/rerun/non-root-trial/not-fbound)
  (test-try/rerun/non-root-trial))

(deftest test-try/rerun/non-root-trial/with-test ()
  (let ((trial (try (lambda () (with-test (t0))) :print nil :collect t)))
    (try (trial (first (children trial))) :print nil)))

(deftest test-try/rerun/non-root-trial/not-fbound ()
  (%with-deftest ((test-name "try::%not-test-bound-p") ())
    (let ((trial (try `(,test-name) :print nil :collect t)))
      (fmakunbound test-name)
      (signals (error :pred "no longer FBOUNDP")
        (try (trial (first (children trial))))))))

(deftest test-try/rerun/non-root-trial ()
  (let* ((trial (try '(%is/success %simple-success) :print nil :collect t))
         (trial (try
                 ;; This is the %SIMPLE-SUCCESS (reverse order).
                 (trial (first (children trial)))
                 :print nil)))
    (is (passedp trial))
    (is (typep (verdict trial) 'verdict-skip))))


(deftest test-try/rerun/skipping ()
  (test-try/rerun/skip-success/collected)
  (test-try/rerun/skip-success/not-collected)
  (test-try/rerun/skip/trial-fail)
  (test-try/rerun/trial-fail-and-trial-start)
  (test-try/rerun/children-with-same-name)
  (test-try/rerun/children-with-same-name-in-reverse-order)
  (test-try/rerun/shuffle))

(deftest %outer-failure-inner-success ()
  (with-test (t0)
    (with-test (t1)
      (is t))
    (is nil)))

(deftest test-try/rerun/skip-success/collected ()
  ;; T1 is successful and is collected. Rerun skips it.
  (dolist (rerun '(unexpected failure))
    (check-try-output ((try '%outer-failure-inner-success
                            :print nil :collect t)
                       :rerun rerun)
                      "%OUTER-FAILURE-INNER-SUCCESS
  T0
    - T1
    × (IS NIL)
  × T0 ×1
× %OUTER-FAILURE-INNER-SUCCESS ×1
")))

(deftest test-try/rerun/skip-success/not-collected ()
  ;; T1 is successful and is not collected. Rerun skips it.
  (dolist (rerun '(unexpected failure))
    (check-try-output ((try '%outer-failure-inner-success :print nil
                            :collect 'unexpected)
                       :rerun rerun)
                      "%OUTER-FAILURE-INNER-SUCCESS
  T0
    - T1
    × (IS NIL)
  × T0 ×1
× %OUTER-FAILURE-INNER-SUCCESS ×1
")))

(deftest test-try/rerun/skip/trial-fail ()
  (dolist (rerun '(unexpected failure))
    (check-try-output ((try '%outer-failure-inner-success :print nil
                            ;; collect the failed trials only
                            :collect '(and verdict (not pass)))
                       :rerun rerun)
                      "%OUTER-FAILURE-INNER-SUCCESS
  T0
    - T1
    × (IS NIL)
  × T0 ×1
× %OUTER-FAILURE-INNER-SUCCESS ×1
")))

(deftest test-try/rerun/trial-fail-and-trial-start ()
  (dolist (rerun '((not pass) trial-start))
    (check-try-output ((try '%outer-failure-inner-success :print nil
                            ;; collect the failed trials only
                            :collect '(and verdict (not pass)))
                       :rerun rerun)
                      "%OUTER-FAILURE-INNER-SUCCESS
  T0
    - T1
    × (IS NIL)
  × T0 ×1
× %OUTER-FAILURE-INNER-SUCCESS ×1
")))

(deftest %children-with-same-name ()
  (with-test (c)
    (with-test (sub)
      (is nil)))
  (with-test (c)))

(deftest %children-with-same-name-in-reverse-order ()
  (with-test (c))
  (with-test (c)
    (with-test (sub)
      (is nil))))

(deftest test-try/rerun/children-with-same-name ()
  (dolist (rerun '(unexpected failure))
    (check-try-output ((try '%children-with-same-name :print nil
                            :collect t)
                       :rerun rerun
                       :msg (list "~S ~S works" :rerun rerun))
                      "%CHILDREN-WITH-SAME-NAME
  C
    SUB
      × (IS NIL)
    × SUB ×1
  × C ×1
  - C
× %CHILDREN-WITH-SAME-NAME ×1
")))

(deftest test-try/rerun/children-with-same-name-in-reverse-order ()
  (dolist (rerun '(unexpected failure))
    (check-try-output ((try '%children-with-same-name-in-reverse-order
                            :print nil :collect t)
                       :rerun rerun
                       :msg (list "~S ~S works" :rerun rerun))
                      "%CHILDREN-WITH-SAME-NAME-IN-REVERSE-ORDER
  - C
  C
    SUB
      × (IS NIL)
    × SUB ×1
  × C ×1
× %CHILDREN-WITH-SAME-NAME-IN-REVERSE-ORDER ×1
")))

(defvar *reverse-children* nil)

(deftest %children-with-different-names ()
  (cond (*reverse-children*
         (with-test (t1)
           (is t))
         (with-test (t0)
           (is nil)))
        (t
         (with-test (t0)
           (is nil))
         (with-test (t1)
           (is t)))))

(deftest test-try/rerun/shuffle ()
  (let ((trial (try '%children-with-different-names :print nil :collect t)))
    ;; Change the order as if we had WITH-SHUFFLING in there.
    (let ((*reverse-children* t))
      (check-try-output (trial)
                        "%CHILDREN-WITH-DIFFERENT-NAMES
  - T1
  T0
    × (IS NIL)
  × T0 ×1
× %CHILDREN-WITH-DIFFERENT-NAMES ×1
"))))


(deftest test-try/rerun/retry ()
  (check-try-output ((try (named-lambda-test %retry-and-rerun ()
                            (let ((n 0))
                              (with-test (t0)
                                (cond ((= n 0)
                                       (incf n)
                                       (retry-trial t0))
                                      (t
                                       (is nil))))))
                          :print nil))
                    "%RETRY-AND-RERUN
  T0 retry #1
    × (IS NIL)
  × T0 ×1
× %RETRY-AND-RERUN ×1
"))


(deftest test-try/rerun/implicit ()
  (test-try/rerun/implicit/with-test)
  (test-try/rerun/implicit/failure)
  (test-try/rerun/implicit-with-args/failure))

(deftest test-try/rerun/implicit/with-test ()
  (check-try-output ((with-silent-implicit-try (with-test (t0)
                                                 (is nil))))
                    "T0
  × (IS NIL)
× T0 ×1
")
  (is (equal (with-std-try
               (let ((*print-duration*))
                 (with-silent-implicit-try
                   (let ((*print* t))
                     (with-output-to-string (*stream*)
                       (funcall (with-test (t0)
                                  (is nil))))))))
             "T0
  × (IS NIL)
× T0 ×1
T0
  × (IS NIL)
× T0 ×1
")
      :msg "Implicit TRY rerun implicitly."))

(deftest %simple-failure ()
  (is nil))

(deftest test-try/rerun/implicit/failure ()
  (with-test ("explicit TRY with named trial => named trial")
    (let ((trial (try '%simple-failure :print nil)))
      (is (try::named-trial-p trial))
      (is (equal (try::cform trial) '(%simple-failure)))))
  ;; Check that an implicit call to TRY by %SIMPLE-FAILURE will behave
  ;; the same.
  (let ((trial (with-silent-implicit-try (%simple-failure))))
    (is (try::named-trial-p trial))
    (is (not (passedp (try trial :print nil))))
    ;; Same with FUNCALL on TRIAL.
    (check-implicit-try-output ((funcall trial))
                               "%SIMPLE-FAILURE
  × (IS NIL)
× %SIMPLE-FAILURE ×1
")))

(deftest %simple-failure-with-args (x)
  (is (< x x)))

(deftest test-try/rerun/implicit-with-args/failure ()
  ;; Must wrap it in LAMBDA to pass args to it with explicit TRY.
  (let ((trial (try (lambda () (%simple-failure-with-args 5)) :print nil)))
    (is (try::try-trial-p trial)))
  ;; With implicit TRY, that's not required.
  (let ((trial (with-silent-implicit-try (%simple-failure-with-args 5))))
    (is (try::named-trial-p trial))
    (is (not (passedp (try trial :print nil))))
    (check-implicit-try-output ((funcall trial))
                               "%SIMPLE-FAILURE-WITH-ARGS
  × (IS (< X X))
    where
      X = 5
× %SIMPLE-FAILURE-WITH-ARGS ×1
")))
