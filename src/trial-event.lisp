(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @trial-events (:title "Trial Events")
  (trial-event condition)
  (trial (reader trial-event))
  (trial-start condition)
  (verdict condition)
  (expected-verdict-success condition)
  (unexpected-verdict-success condition)
  (expected-verdict-failure condition)
  (unexpected-verdict-failure condition)
  (verdict-skip condition)
  (verdict-abort* condition))

(define-condition trial-event (event)
  ((trial
    :initarg :trial :reader trial
    :documentation "The TRIAL object associated with this event."))
  (:documentation "A TRIAL-EVENT is either a TRIAL-START or a
  VERDICT."))

(define-condition trial-start (trial-event) ()
  (:documentation """TRIAL-START is signalled when a test function
  (see @TESTS) is entered and a TRIAL is started, it is already the
  CURRENT-TRIAL, and the @TRIAL-RESTARTS are available. It is also
  signalled when a trial is retried:

  ```cl-transcript
  (let ((*print* nil)
        (n 0))
    (with-test ()
      (handler-bind ((trial-start (lambda (c)
                                    (format t "TRIAL-START for ~S retry#~S~%"
                                            (test-name (trial c))
                                            (n-retries (trial c))))))
        (with-test (this)
          (incf n)
          (when (< n 3)
            (retry-trial))))))
  .. TRIAL-START for THIS retry#0
  .. TRIAL-START for THIS retry#1
  .. TRIAL-START for THIS retry#2
  ..
  ```

  The matching of TRIAL-START events is less straightforward than that
  of other EVENTs.

  - When a TRIAL-START event matches the `COLLECT` type (see
    @COLLECT), its [TRIAL][(reader trial-event)] is collected.

  - Similarly, when a TRIAL-START matches the [PRINT][argument]
    type (see @PRINT), it is printed immediately, and its trial's
    VERDICT will be printed too regardless of whether it matches
    [PRINT][argument]. If TRIAL-START does not match
    [PRINT][argument], it may still be printed if for example
    *PRINT-PARENT* requires it.

  - When a TRIAL-START matches the `RERUN` type (see @RERUN), its
    [TRIAL][(reader trial-event)] may be rerun.

  - Also, see WITH-SKIP.
  """))

(defmethod write-event ((trial-start trial-start) stream &key terse ctx)
  (declare (ignore terse ctx))
  (format stream "~S" (ignore-errors (trial trial-start))))

(define-condition verdict (trial-event outcome)
  ((elapsed-seconds :initform nil :initarg :elapsed-seconds
                    :reader elapsed-seconds))
  (:documentation "A VERDICT is the OUTCOME of a TRIAL. It is one of
  `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}`, VERDICT-SKIP and
  VERDICT-ABORT*. Regarding how the verdict type is determined, see
  @TRIAL-VERDICTS.

  Verdicts are signalled while their [TRIAL][(reader trial-event)] is
  still the CURRENT-TRIAL, and @TRIAL-RESTARTS are still available.

  ```cl-transcript (:check-consistency nil)
  (try (lambda ()
         (handler-bind (((and verdict failure) #'retry-trial))
           (with-test (this)
             (is (zerop (random 2)))))))
  .. (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>)
  ..   THIS
  ..     ⊠ (IS (ZEROP #1=(RANDOM 2)))
  ..       where
  ..         #1# = 1
  ..   THIS retry #1
  ..     ⋅ (IS (ZEROP (RANDOM 2)))
  ..   ⋅ THIS ⋅1
  .. ⋅ (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) ⋅1
  ..
  ==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) EXPECTED-SUCCESS 0.000s ⋅1>
  ```
 "))

(declaim (ftype (function (t t) t) write-trial-counts))
(declaim (ftype (function (t) t) test-name))

(defmethod write-event ((verdict verdict) stream &key terse ctx)
  (if (and terse ctx)
      (let ((trial (trial verdict)))
        (if (stringp (test-name trial))
            (format stream "~A" (test-name trial))
            (format stream "~S" (test-name trial)))
        (write-trial-counts trial stream))
      (format stream "~S" (ignore-errors (trial verdict)))))

(define-combi-event (expected verdict success))
(define-combi-event (unexpected verdict success))
(define-combi-event (expected verdict failure))
(define-combi-event (unexpected verdict failure))
(define-combi-event (verdict skip))
(define-condition verdict-abort* (verdict abort* dismissal) ())

;;; KLUDGE: For SUBTYPEP to work in DETERMINE-OUTCOME-TYPE. Not to be
;;; exported because for example EXPECTED-RESULT-SUCCESS is not a
;;; subtype of RESULT-SUCCESS.
(define-combi-event (verdict success))
(define-combi-event (verdict failure))
