(in-package :try-test)

(deftest test-trial ()
  (test-trial/var-and-name)
  (test-trial/return)
  (test-with-test/nlx)
  (test-with-test/declaration)
  (test-with-test/restarts)
  (test-deftest)
  (test-visible-restarts)
  (test-registry)
  (test-set-try-debug)
  (test-edge-cases))


;;; Here and elsewhere, there are four ways to enter a trial:
;;;
;;; 1. Through WITH-TEST while already running under TRY
;;;
;;; 2. Through WITH-TEST that starts TRY (this is faked with
;;;    WITH-SILENT-IMPLICIT-TRY).
;;;
;;; 3. Through DEFTEST while already running under TRY.
;;;
;;; 4. Through DEFTEST called by TRY.
(deftest test-trial/var-and-name ()
  (with-test ("trial var and name in WITH-TEST")
    (with-test ()
      (is (null (test-name (current-trial)))))
    (with-test (t0)
      (is (try::trialp t0))
      (is (eq (test-name t0) 't0)))
    (with-test (nil :name "name")
      (is (equal (test-name (current-trial)) "name")))
    (with-test (t0 :name "name")
      (is (try::trialp t0))
      (is (equal (test-name t0) "name")))
    (with-test ("name")
      (is (equal (test-name (current-trial)) "name")))
    (signals (error :pred "mutually exclusive")
      (macroexpand '(with-test ("name" :name "other-name")))))
  (with-test ("trial var and name in implicit TRY")
    (is (passedp (with-silent-implicit-try
                   (with-test ()
                     (is (null (test-name (current-trial))))))))
    (is (passedp (with-silent-implicit-try
                   (with-test (t0)
                     (is (try::trialp t0))
                     (is (eq (test-name t0) 't0))))))
    (is (passedp (with-silent-implicit-try
                   (with-test (t0 :name "name")
                     (is (try::trialp t0))
                     (is (equal (test-name t0) "name"))))))
    (is (passedp (with-silent-implicit-try
                   (with-test ("name")
                     (is (equal (test-name (current-trial)) "name")))))))
  (with-test ("trial var and name in DEFTEST")
    (is (passedp (call-deftest (foo77 ()
                                 (is (try::trialp foo77))
                                 (is (eq (test-name foo77) 'foo77)))))))
  (with-test ("trial var and name in explicit TRY")
    (is (passedp (try (named-lambda-test t0 ()
                        (is (try::trialp t0))
                        (is (eq (test-name t0) 't0)))
                      :print nil)))
    (is (passedp (try (list
                       (named-lambda-test t0 ()
                         (is (try::trialp t0))
                         (is (eq (test-name t0) 't0)))
                       (named-lambda-test t1 ()
                         (is (try::trialp t1))
                         (is (eq (test-name t1) 't1))))
                      :print nil)))))

(deftest test-trial/return ()
  (with-test ("implicit return from a TRIAL returns itself")
    (is (match-values (with-test (t0) 7)
          (try::trialp *)))
    (is (match-values (with-silent-implicit-try (with-test (t0) 7))
          (try::trialp *)))
    (is (match-values (call-deftest (foo77 () 7))
          (try::trialp *)))
    (is (match-values (try (named-lambda-test foo77 () 7) :print nil)
          (try::trialp *)))
    ;; Wrapped in extra trial.
    (is (match-values (try (lambda () (with-test (t0) 7)) :print nil)
          (try::trialp *))))
  (with-test ("RETURN-FROM a TRIAL returns extra values")
    (is (match-values (with-test (t0)
                        (return-from t0 (values 1 2)))
          (try::trialp *) (= * 1) (= * 2)))
    (is (match-values (with-silent-implicit-try
                        (with-test (t0)
                          (return-from t0 (values 1 2))))
          (try::trialp *) (= * 1) (= * 2)))
    (is (match-values (call-deftest (foo77 ()
                                      (return-from foo77 (values 1 2))))
          (try::trialp *) (= * 1) (= * 2)))
    (is (match-values (try (named-lambda-test foo77 ()
                             (return-from foo77 (values 1 2)))
                           :print nil)
          (try::trialp *) (= * 1) (= * 2)))
    ;; No way to return values through the wrapper trial.
    (is (match-values (try (lambda ()
                             (with-test (t0)
                               (return-from t0 (values 1 2))))
                           :print nil)
          (try::trialp *))))
  (with-test ("RETURN from an anonymous TRIAL returns extra values")
    (is (match-values (with-test ()
                        (return (values 1 2)))
          (try::trialp *) (= * 1) (= * 2)))
    (is (match-values (with-silent-implicit-try
                        (with-test ()
                          (return (values 1 2))))
          (try::trialp *) (= * 1) (= * 2)))))


(deftest test-with-test/nlx ()
  (test-with-test/success/cerror)
  (test-with-test/failure/nlx)
  (test-with-test/failure/error)
  (test-with-test/failure/debugger)
  (test-with-test/failure/nested-with-tests)
  (test-with-test/cancel-nlx-on-error)
  (test-with-test/multiple-unexpected-errors)
  (test-with-test/check-after-unexpected-error)
  (test-with-test/check-after-skip)
  (test-with-test/cancel-nlx-of-skip)
  (test-with-test/check-after-retry)
  (test-with-test/cancel-nlx-of-retry/outside)
  (test-with-test/cancel-nlx-of-retry/inside)
  (test-with-test/abort-after-skip)
  (test-with-test/retry-after-skip)
  (test-with-test/abort-trial-on-trial-start)
  (test-with-test/skip-trial-on-trial-start)
  (test-with-test/retry-trial-on-trial-start)
  (test-with-test/abort-trial-on-result)
  (test-with-test/skip-trial-on-result)
  (test-with-test/retry-trial-on-result)
  (test-with-test/abort-trial-on-general-condition)
  (test-with-test/skip-trial-on-general-condition)
  (test-with-test/retry-trial-on-general-condition)
  (test-with-test/abort-trial-on-verdict)
  (test-with-test/skip-trial-on-verdict)
  (test-with-test/retry-trial-on-verdict))

(deftest test-with-test/success/cerror ()
  (with-test (t0)
    (handler-bind ((error #'continue))
      (cerror "cont" "xxx"))))

(deftest test-with-test/failure/nlx ()
  (with-test ()
    (signals (nlx :pred "a non-local exit")
      (catch 'xxx
        (with-test (t0)
          (throw 'xxx nil))))))

(deftest test-with-test/failure/error ()
  (check-try-output ('%simple-unexpected-error :describe nil)
                    "%SIMPLE-UNEXPECTED-ERROR
  ! \"my-msg\" (MY-ERR)
! %SIMPLE-UNEXPECTED-ERROR !1
"))

(deftest test-with-test/failure/debugger ()
  (check-try-output ((named-lambda-test foo77 ()
                       (invoke-debugger (make-condition 'my-err)))
                     :describe nil)
                    "FOO77
  ! debugger invoked on \"my-msg\" (MY-ERR)
! FOO77 !1
")
  (check-try-output ((named-lambda-test foo77 ()
                       ;; ERROR with a non-ERROR condition ends up in
                       ;; the debugger too.
                       (error (make-condition 'my-cond)))
                     :describe nil)
                    "FOO77
  ! debugger invoked on \"my-msg\" (MY-COND)
! FOO77 !1
"))

(deftest test-with-test/failure/nested-with-tests ()
  (check-outcomes (lambda ()
                    (with-test (t0)
                      (with-test (t1)
                        (error 'my-err))))
                  '((abort* 1))))

;;; Arguably, this tests a shortcoming: there is no way to detect
;;; whether WITH-TEST unwinds due to an error or the unwinding caused
;;; by the error handler was cancelled by an throw.
(deftest test-with-test/cancel-nlx-on-error ()
  (check-try-output ('%cancel-nlx-of-unexpected-error/catch-outside
                     :describe nil)
                    "%CANCEL-NLX-OF-UNEXPECTED-ERROR/CATCH-OUTSIDE
  FFF
    ! \"my-msg\" (MY-ERR)
  ! FFF !1
× %CANCEL-NLX-OF-UNEXPECTED-ERROR/CATCH-OUTSIDE !1
")
  (check-try-output ('%cancel-nlx-of-unexpected-error/catch-inside
                     :describe nil)
                    "%CANCEL-NLX-OF-UNEXPECTED-ERROR/CATCH-INSIDE
  FFF
    ! \"my-msg\" (MY-ERR)
  ! FFF !1
× %CANCEL-NLX-OF-UNEXPECTED-ERROR/CATCH-INSIDE !1
"))

(deftest %cancel-nlx-of-unexpected-error/catch-outside ()
  (catch 'foo
    (with-test (fff)
      (unwind-protect
           (error 'my-err)
        (throw 'foo nil)))))

(deftest %cancel-nlx-of-unexpected-error/catch-inside ()
  (with-test (fff)
    (catch 'foo
      (unwind-protect
           (error 'my-err)
        (throw 'foo nil)))))

(deftest test-with-test/multiple-unexpected-errors ()
  (check-outcomes (lambda ()
                    (unwind-protect
                         (error "xxx")
                      (error "yyy")))
                  '((abort* 2))))

(deftest test-with-test/check-after-unexpected-error ()
  (check-outcomes (lambda ()
                    (unwind-protect
                         (error "xxx")
                      (is t)))
                  '((abort* 1)
                    (expected-success 1))))

(deftest test-with-test/check-after-skip ()
  (check-try-output ('%skip-outer)
                    "%SKIP-OUTER
  OUTER
    INNER
      ✓ before skip
      ✓ after skip
    - INNER ✓2
  - OUTER ✓2
✓ %SKIP-OUTER ✓2
"))

(deftest %skip-outer ()
  (with-test (outer)
    (with-test (inner)
      (is t :msg "before skip")
      (unwind-protect
           (skip-trial nil outer)
        (is t :msg "after skip")))
    ;; Not Executed.
    (is nil)))

(deftest test-with-test/cancel-nlx-of-skip ()
  (check-try-output ('%cancel-nlx-of-skip/outside
                     :print 'outcome
                     :describe nil)
                    "%CANCEL-NLX-OF-SKIP/OUTSIDE
  - FFF
✓ %CANCEL-NLX-OF-SKIP/OUTSIDE
")
  (check-try-output ('%cancel-nlx-of-skip/inside
                     :describe nil)
                    "%CANCEL-NLX-OF-SKIP/INSIDE
  FFF
    ✓ (IS T)
  - FFF ✓1
✓ %CANCEL-NLX-OF-SKIP/INSIDE ✓1
"))

(deftest %cancel-nlx-of-skip/outside ()
  (catch 'foo
    (with-test (fff)
      (unwind-protect
           (skip-trial)
        (throw 'foo nil)))))

(deftest %cancel-nlx-of-skip/inside ()
  (with-test (fff)
    (catch 'foo
      (unwind-protect
           (skip-trial)
        (throw 'foo nil)))
    (is t)))

(deftest test-with-test/check-after-retry ()
  (check-try-output ('%retry-outer)
                    "%RETRY-OUTER
  OUTER
    ✓ (IS T)
    INNER
      ✓ after skip
    - INNER ✓1
  OUTER retry #1
    ✓ (IS T)
    ✓ INNER
  ✓ OUTER ✓1
✓ %RETRY-OUTER ✓1
"))

(deftest %retry-outer ()
  (let ((n 0))
    (with-test (outer)
      (is t)
      (with-test (inner)
        (when (= n 0)
          (incf n)
          (unwind-protect
               (retry-trial nil outer)
            (is t :msg "after skip")))))))

(deftest test-with-test/cancel-nlx-of-retry/outside ()
  (check-try-output ('%cancel-nlx-of-retry/outside
                     :describe nil)
                    "%CANCEL-NLX-OF-RETRY/OUTSIDE
  FFF
    ! non-local exit (which cancelled the nlx of RETRY-TRIAL)
  ! FFF !1
× %CANCEL-NLX-OF-RETRY/OUTSIDE !1
"))

(deftest %cancel-nlx-of-retry/outside ()
  (catch 'foo
    (with-test (fff)
      (unwind-protect
           (retry-trial)
        (throw 'foo nil)))))

(deftest test-with-test/cancel-nlx-of-retry/inside ()
  (check-try-output ('%cancel-nlx-of-retry/inside
                     :describe nil)
                    "%CANCEL-NLX-OF-RETRY/INSIDE
  FFF
    ✓ before retry-trial
    ✓ after retry-trial
  FFF retry #1
    ✓ before retry-trial
    ✓ after retry-trial
  ✓ FFF ✓2
✓ %CANCEL-NLX-OF-RETRY/INSIDE ✓2
"))

(deftest %cancel-nlx-of-retry/inside ()
  (let ((n 0))
    (with-test (fff)
      (is t :msg "before retry-trial")
      (catch 'foo
        (incf n)
        (when (= n 1)
          (unwind-protect
               (retry-trial)
            (throw 'foo nil))))
      (is t :msg "after retry-trial"))))

(deftest test-with-test/abort-after-skip ()
  (check-try-output ('%abort-after-skip :describe nil)
                    "%ABORT-AFTER-SKIP
  T0
    ! \"my-msg\" (MY-ERR)
  ! T0 !1
× %ABORT-AFTER-SKIP !1
"))

(deftest %abort-after-skip ()
  (with-test (t0)
    (unwind-protect
         (skip-trial)
      (error 'my-err))))

(deftest test-with-test/retry-after-skip ()
  (let ((trial (check-try-output ('%retry-after-skip :describe nil
                                  :collect t)
                                 "%RETRY-AFTER-SKIP
  T0
    ✓ true 1
  T0 retry #1
    ✓ true 2
  ✓ T0 ✓1
✓ %RETRY-AFTER-SKIP ✓1
")))
    (check-try-output (trial :replay-events t)
                      "%RETRY-AFTER-SKIP
  T0 retry #1
    ✓ true 2
  ✓ T0 ✓1
✓ %RETRY-AFTER-SKIP ✓1
"))
  (check-try-output ('%retry-after-skip :print 'trial-start :describe nil)
                    "%RETRY-AFTER-SKIP
  T0
  T0 retry #1
  ✓ T0 ✓1
✓ %RETRY-AFTER-SKIP ✓1
"))

(deftest %retry-after-skip ()
  (let ((n 0))
    (with-test (t0)
      (incf n)
      (is t :msg ("true ~S" n))
      (when (= n 1)
        (unwind-protect
             (skip-trial)
          (retry-trial))))))

(deftest test-with-test/abort-trial-on-trial-start ()
  (let ((trial (trial (first (children (try '%abort-on-trial-start
                                            :print nil))))))
    (is (try::trialp trial))
    (is (typep (verdict trial) 'verdict-abort*))))

(deftest %abort-on-trial-start ()
  (handler-bind ((trial-start #'abort-trial))
    (with-test (t0))))

(deftest test-with-test/skip-trial-on-trial-start ()
  (let ((trial (handler-bind ((trial-start #'skip-trial))
                 (with-test (t0)))))
    (is (try::trialp trial))
    (is (typep (verdict trial) 'verdict-skip))))

(deftest test-with-test/retry-trial-on-trial-start ()
  (check-try-output ((named-lambda-test foo77 ()
                       (let ((n 0))
                         (handler-bind ((trial-start (lambda (c)
                                                       (incf n)
                                                       (when (= n 1)
                                                         (retry-trial c)))))
                           (with-test (t0)))))
                     :print t)
                    "FOO77
  T0
  T0 retry #1
  ✓ T0
✓ FOO77
"))

(deftest test-with-test/abort-trial-on-result ()
  (check-try-output ((named-lambda-test t0 ()
                       (handler-bind ((result #'abort-trial))
                         (is nil))))
                    "T0
  × (IS NIL)
! T0 ×1
"))

(deftest test-with-test/skip-trial-on-result ()
  (check-try-output ((named-lambda-test t0 ()
                       (handler-bind ((result #'skip-trial))
                         (is nil))))
                    "T0
  × (IS NIL)
- T0 ×1
"))

(deftest test-with-test/retry-trial-on-result ()
  (check-try-output ((named-lambda-test foo77 ()
                       (let ((n 0))
                         (with-test (t0)
                           (handler-bind ((failure #'retry-trial))
                             (incf n)
                             (is (= n 2)))))))
                    "FOO77
  T0
    × (IS (= N 2))
      where
        N = 1
  T0 retry #1
    ✓ (IS (= N 2))
  ✓ T0 ✓1
✓ FOO77 ✓1
"))

(deftest test-with-test/abort-trial-on-general-condition ()
  (let ((trial (try (named-lambda-test foo77 ()
                      (handler-bind ((my-cond #'abort-trial))
                        (signal 'my-cond)))
                    :print nil)))
    (is (try::trialp trial))
    (is (typep (verdict trial) 'verdict-abort*))))

(deftest test-with-test/skip-trial-on-general-condition ()
  (let ((trial (with-test (t0)
                 (handler-bind ((my-cond #'skip-trial))
                   (signal 'my-cond)))))
    (is (try::trialp trial))
    (is (typep (verdict trial) 'verdict-skip))))

(deftest test-with-test/retry-trial-on-general-condition ()
  (check-try-output ((named-lambda-test foo77 ()
                       (let ((n 0))
                         (with-test (t0)
                           (handler-bind ((my-cond (lambda (c)
                                                     (declare (ignore c))
                                                     (incf n)
                                                     (when (= n 1)
                                                       (retry-trial)))))
                             (signal 'my-cond)))))
                     :print t)
                    "FOO77
  T0
  T0 retry #1
  ✓ T0
✓ FOO77
"))

(deftest test-with-test/abort-trial-on-verdict ()
  (test-with-test/abort-trial-on-verdict-0)
  (test-with-test/abort-outer-trial-on-verdict-1)
  (test-with-test/abort-outer-trial-on-verdict-2)
  (test-with-test/abort-outer-trial-on-verdict-3))

(deftest test-with-test/abort-trial-on-verdict-0 ()
  (check-try-output ((named-lambda-test t0 ()
                       (handler-bind ((unexpected-verdict-failure
                                        #'abort-trial))
                         (with-test ()
                           (is nil)))))
                    "T0
  NIL
    × (IS NIL)
  ! NIL ×1
× T0 ×1
"))

(deftest test-with-test/abort-outer-trial-on-verdict-1 ()
  (check-try-output ((named-lambda-test outer ()
                       (handler-bind ((verdict
                                        (lambda (c)
                                          (abort-trial c outer))))
                         (with-test (inner)
                           (is t)))))
                    "OUTER
  INNER
    ✓ (IS T)
  - INNER ✓1
! OUTER ✓1
"))

(deftest test-with-test/abort-outer-trial-on-verdict-2 ()
  (check-try-output ((named-lambda-test outer ()
                       (handler-bind ((verdict
                                        (lambda (c)
                                          (abort-trial nil outer))))
                         (with-test (inner)
                           (is t)))))
                    "OUTER
  INNER
    ✓ (IS T)
  - INNER ✓1
! OUTER ✓1
"))

(deftest test-with-test/abort-outer-trial-on-verdict-3 ()
  (check-try-output ((named-lambda-test outer ()
                       (catch 'xxx
                         (handler-bind ((verdict
                                          (lambda (c)
                                            (declare (ignore c))
                                            (throw 'xxx nil))))
                           (with-test (inner)
                             (is t))))))
                    "OUTER
  INNER
    ✓ (IS T)
  ! INNER ✓1
× OUTER ✓1
"))

(deftest test-with-test/skip-trial-on-verdict ()
  (check-try-output ((named-lambda-test t0 ()
                       (handler-bind ((unexpected-verdict-failure
                                        #'skip-trial))
                         (with-test ()
                           (is nil)))))
                    "T0
  NIL
    × (IS NIL)
  - NIL ×1
✓ T0 ×1
"))

(deftest test-with-test/retry-trial-on-verdict ()
  (check-try-output ((named-lambda-test t0 ()
                       (let ((n 0))
                         (handler-bind ((unexpected-verdict-failure
                                          #'retry-trial))
                           (with-test ()
                             (incf n)
                             (is (= n 2)))))))
                    "T0
  NIL
    × (IS (= N 2))
      where
        N = 1
  NIL retry #1
    ✓ (IS (= N 2))
  ✓ NIL ✓1
✓ T0 ✓1
"))


(deftest test-with-test/declaration ()
  (with-test (t0)
    (declare (optimize speed))))


(deftest test-with-test/restarts ()
  (test-with-test/skip)
  (test-with-test/skip/with-skip)
  (test-with-test/failure/retry)
  (test-signal-check/record-event-restart))

(deftest %test-with-skip-trial ()
  (signals (verdict-skip :name 'with-test-with-skip :handler nil)
    (with-test (sdf)
      ;; This should not be counted.
      (is t)
      (skip-trial)
      ;; This should not be reached.
      (is nil))))

(deftest test-with-test/skip ()
  (check-try-output ('%test-with-skip-trial)
                    "%TEST-WITH-SKIP-TRIAL
  SDF
    ✓ (IS T)
  - SDF ✓1
  ✓ WITH-TEST-WITH-SKIP signals a condition of type VERDICT-SKIP.
✓ %TEST-WITH-SKIP-TRIAL ✓2
"))

(deftest %test-with-with-skip ()
  (signals (verdict-skip :name 'with-test-in-with-skip :handler nil)
    (with-skip ()
      (with-test (sdf)
        ;; This should not be reached.
        (is nil)))))

(deftest test-with-test/skip/with-skip ()
  (check-try-output ('%test-with-with-skip
                     :print '(or outcome verdict-skip))
                    "%TEST-WITH-WITH-SKIP
  - SDF
  ✓ WITH-TEST-IN-WITH-SKIP signals a condition of type VERDICT-SKIP.
✓ %TEST-WITH-WITH-SKIP ✓1
"))

(deftest test-with-test/failure/retry ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (signals (expected-success :handler nil)
                               (signals-not (error*)
                                 (let ((x 0))
                                   (handler-bind ((nlx #'retry-trial))
                                     (catch 'xxx
                                       (with-test (t1)
                                         (incf x)
                                         (when (= x 1)
                                           (throw 'xxx nil))))
                                     47)))))))
                  '((expected-success 3))))

(deftest test-signal-check/record-event-restart ()
  (let ((record-event-restart nil))
    ;; If not in TRY, there is nothing to record hence no RECORD-EVENT
    ;; restart. Instead, there is a CONTINUE restart.
    (with-new-implicit-try
      (handler-bind ((success
                       (lambda (c)
                         (setq record-event-restart
                               (find-restart 'record-event))
                         (continue c))))
        (is t)))
    (is (null record-event-restart))))


(deftest test-deftest ()
  (test-deftest/docstring)
  (test-deftest/declare)
  (test-deftest/docstring-and-declare)
  (test-deftest/registry))

(deftest %deftest/docstring ()
  "docstring"
  (is t))

(deftest test-deftest/docstring ()
  (%deftest/docstring)
  (with-failure-expected ((alexandria:featurep :clisp))
    (is (equal (documentation '%deftest/docstring 'function) "docstring"))))

(deftest %deftest/declare (&optional (x 7))
  (declare (type fixnum x)))

(deftest test-deftest/declare ()
  (is (passedp (%deftest/declare))))

(deftest %deftest/docstring-and-declare (&optional (x 7))
  "docstring"
  (declare (type fixnum x)))

(deftest test-deftest/docstring-and-declare ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (is (equal (documentation '%deftest/docstring-and-declare 'function)
               "docstring")))
  (is (passedp (%deftest/docstring-and-declare))))

(deftest test-deftest/registry ()
  (%with-deftest-registry
    (%with-deftest ((test-name "try::%some-test") ())
      (is (test-bound-p test-name))
      ;; Silence any redifinition warnings.
      (handler-bind ((warning #'muffle-warning))
        (eval `(defun ,test-name ())))
      (is (not (test-bound-p test-name)))))
  (%with-deftest-registry
    (%with-deftest ((test-name "try::%some-test") ())
      (is (test-bound-p test-name))
      (fmakunbound test-name)
      (is (not (test-bound-p test-name)))))
  (%with-deftest-registry
    (%with-deftest ((test-name "try::%some-test") ())
      (is (test-bound-p test-name))
      (unintern test-name (symbol-package test-name))
      (is (not (test-bound-p test-name)))))
  (with-test ("TEST-BOUND-P vs locked package")
    (is (null (test-bound-p 'print)))))


(deftest test-visible-restarts ()
  (is (null (% (intersection (active-try-restarts)
                             (active-try-restarts-in-nested-try))))
      :msg ("Outer ~S restarts are not visible in nested ~S."
            'try 'try))
  (is (null (% (intersection
                (active-try-restarts)
                ;; Invoke a nested TRY in a condition handler for
                ;; SUCCESS.
                (catch 'nlx
                  (handler-bind ((success
                                   (lambda (c)
                                     (declare (ignore c))
                                     (throw 'nlx
                                       (active-try-restarts-in-nested-try)))))
                    (is t)
                    ;; Muffle style warning about (IS T) => T being an
                    ;; bad argument for INTERSECTION.
                    nil)))))
      :msg ("Outer ~S restarts for ~S are not visible in nested ~S."
            'try 'success 'try))
  (is (null (% (intersection
                (active-try-restarts)
                (catch 'nlx
                  (handler-bind ((failure
                                   (lambda (c)
                                     (declare (ignore c))
                                     (throw 'nlx
                                       (active-try-restarts-in-nested-try)))))
                    (is nil)
                    nil)))))
      :msg ("Outer ~S restarts for ~S are not visible in nested ~S."
            'try 'failure 'try))
  (is (null (% (intersection
                (active-try-restarts)
                (catch 'nlx
                  (handler-bind ((my-err
                                   (lambda (c)
                                     (declare (ignore c))
                                     (throw 'nlx
                                       (active-try-restarts-in-nested-try)))))
                    (error 'my-err))))))
      :msg ("Outer ~S restarts for ~S are not visible in nested ~S."
            'try 'error 'try))
  (is (null (% (intersection
                (active-try-restarts)
                (catch 'nlx
                  (handler-bind ((nlx
                                   (lambda (c)
                                     (declare (ignore c))
                                     (throw 'nlx
                                       (active-try-restarts-in-nested-try)))))
                    (catch 'foo
                      (with-test (dummy)
                        (throw 'foo nil))))))))
      :msg ("Outer ~S restarts for ~S are not visible in nested ~S."
            'try 'nlx 'try)))

(defun active-try-restarts ()
  (handler-bind ((my-err (lambda (c)
                           (return-from active-try-restarts
                             (remove-if-not #'try-restart-name-p
                                            (compute-restarts c))))))
    (error 'my-err)))

(defun try-restart-name-p (restart)
  (member (restart-name restart)
          '(record-event retry-check skip-check abort-check
            force-expected-success force-unexpected-success
            force-expected-failure force-unexpected-failure
            abort-trial skip-trial retry-trial)))

(defun active-try-restarts-in-nested-try ()
  (let ((restarts nil))
    (with-silent-implicit-try
      (with-test (inner)
        (setq restarts (active-try-restarts))))
    restarts))


(deftest test-registry ()
  (is (endp (list-package-tests (find-package :keyword))))
  (let* ((*package* (find-package :try-test))
         (symbols (list-package-tests (find-package :try-test))))
    (is (member '%simple-success symbols))
    (%with-deftest-registry
      (is (endp (list-package-tests)))
      (%with-deftest ((name-of-test "try-test::%test-in-package") ())
        (is (equal (list-package-tests :try-test)
                   (read-from-string "(try-test::%test-in-package)")))
        (fmakunbound name-of-test)
        (is (not (fboundp name-of-test)))
        (is (not (test-bound-p name-of-test)))
        (is (endp (list-package-tests)))))))


(deftest %set-try-debug ()
  (with-test ()
    (invokes-debugger (t :handler (lambda (c)
                                    (set-try-debug nil)
                                    (abort-trial c)))
      (error 'my-err)))
  (invokes-debugger-not (t)
    ;; Due to (SET-TRY-DEBUG NIL), this does not invoke the debugger
    ;; but is resignalled as UNHANDLED-ERROR, which triggers and
    ;; ABORT-TRIAL.
    (error 'my-err)))

(deftest test-set-try-debug ()
  (is (null (find-restart 'set-try-debug)))
  (check-try-output ('%set-try-debug :describe nil :debug 'unexpected)
                    "%SET-TRY-DEBUG
  NIL
    ! \"my-msg\" (MY-ERR)
    - (ERROR 'MY-ERR) invokes the debugger with a condition of type T.
  ! NIL !1 -1
  ! \"my-msg\" (MY-ERR)
  - (ERROR 'MY-ERR) does not invoke the debugger with a condition of type T.
! %SET-TRY-DEBUG !2 -2
"))


(deftest test-edge-cases ()
  (test-outer-success-inner-failure)
  (test-unexpected-error-backtrace-not-printed-in-debugger)
  (test-with-test/categories-captured))

(deftest %outer-success-inner-failure ()
  (with-test (t1)
    (with-expected-outcome ('(or (and verdict failure)
                              (and result success)))
      (with-test (t2)
        (with-expected-outcome ('success)
          (with-test (t3)
            (is nil)))))))

(deftest test-outer-success-inner-failure ()
  (check-try-output ('%outer-success-inner-failure)
                    "%OUTER-SUCCESS-INNER-FAILURE
  T1
    T2
      T3
        × (IS NIL)
      × T3 ×1
    ×× T2 ×1
  ✓ T1 ×1
✓ %OUTER-SUCCESS-INNER-FAILURE ×1
"))

(deftest test-unexpected-error-backtrace-not-printed-in-debugger ()
  (try (lambda ()
         (let ((unexpected-error-string
                 (catch 'out
                   (try::with-debugger-hook (lambda (c)
                                              (throw 'out (princ-to-string c)))
                     (error "xxx")))))
           ;; Check that the backtrace is not printed.
           (is (< (length unexpected-error-string) 1000))))
       :debug 'unhandled-error
       :describe nil
       :print nil))

(deftest test-with-test/categories-captured ()
  (check-try-output ((named-lambda-test t0 ()
                       (let ((*categories* '((t) (t))))
                         (with-test (t1)
                           (is t)))))
                    "T0
  T1
    ✓ (IS T)
  ✓ T1 ✓1
✓ T0 ✓1
"))
