(in-package :try)

(defsection @try/outcomes (:title "Outcomes")
  (outcome condition)
  (with-expected-outcome macro)
  (with-failure-expected macro)
  (with-skip macro)
  (@try/outcome-restarts section)
  (@try/checks section)
  (@try/trials section))

(define-condition outcome (event) ()
  (:documentation "An OUTCOME is the resolution of either a TRIAL or a
  check (see @TRY/CHECKS), corresponding to subclasses VERDICT and
  RESULT."))


(defvar *expected-outcome* 'success)

(defmacro with-expected-outcome ((expected-type) &body body)
  "When an OUTCOME is to be signalled, EXPECTED-TYPE determines
  whether it's going to be EXPECTED. The concrete OUTCOME classes are
  `{EXPECTED,UNEXPECTED}-{RESULT,VERDICT}-{SUCCESS,FAILURE}` (see
  @TRY/EVENTS), of which RESULT or VERDICT and SUCCESS or FAILURE are
  already known. If a RESULT FAILURE is to be signalled, then the
  moral equivalent of `(SUBTYPEP '(AND RESULT FAILURE) EXPECTED-TYPE)`
  is evaluated and depending on whether it's true,
  EXPECTED-RESULT-FAILURE or UNEXPECTED-RESULT-FAILURE is signalled.

  By default, SUCCESS is expected. The following example shows how to
  expect both SUCCESS and FAILURE for RESULTs, while requiring
  VERDICTs to succeed:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*debug* nil))
    (with-expected-outcome ('(or result (and verdict success)))
      (with-test (t1)
        (is nil))))
  .. T1
  ..   × (IS NIL)
  .. ⋅ T1 ×1
  ..
  ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ×1>
  ```

  This is equivalent to `(WITH-FAILURE-EXPECTED () ...)`. To make
  result failures expected but result successes unexpected:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*debug* nil))
    (with-expected-outcome ('(or (and result failure) (and verdict success)))
      (with-test (t1)
        (is t)
        (is nil))))
  .. T1
  ..   ⊡ (IS T)
  ..   × (IS NIL)
  .. ⋅ T1 ⊡1 ×1
  ..
  ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ⊡1 ×1>
  ```

  This is equivalent to `(WITH-FAILURE-EXPECTED ('FAILURE) ...)`. The
  final example leaves result failures unexpected but makes both
  verdict successes and failures expected:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*debug* nil))
    (with-expected-outcome ('(or (and result success) verdict))
      (with-test (t1)
        (is nil))))
  .. T1
  ..   ⊠ (IS NIL)
  .. × T1 ⊠1
  ..
  ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-FAILURE 0.004s ⊠1>
  ```"
  `(let ((*expected-outcome* ,expected-type))
     ,@body))

(defmacro with-failure-expected ((&optional (result-expected-type t)
                                    (verdict-expected-type ''success))
                                 &body body)
  "A convenience macro on top of WITH-EXPECTED-OUTCOME,
  WITH-FAILURE-EXPECTED expects VERDICTs to have VERDICT-EXPECTED-TYPE
  and RESULTs to have RESULT-EXPECTED-TYPE. A simple
  `(WITH-FAILURE-EXPECTED () ...)` makes all RESULT SUCCESSes and
  FAILUREs EXPECTED. `(WITH-FAILURE-EXPECTED ('FAILURE) ..)` expects
  FAILUREs only, and any SUCCESSes will be UNEXPECTED."
  (with-gensyms (ret vet)
    `(let* ((,ret ,result-expected-type)
            ;; This could be an OR but is written like this to avoid
            ;; unreachable code warning on Allegro.
            (,ret (if ,ret ,ret 'success))
            (,vet ,verdict-expected-type)
            (,vet (if ,vet ,vet 'success)))
       (with-expected-outcome (`(or (and result ,,ret) (and verdict ,,vet)))
         ,@body))))

(defun determine-outcome-type (checkp basic-outcome &optional expected-type)
  (case basic-outcome
    (skip (if checkp 'result-skip 'verdict-skip))
    (abort* (if checkp 'result-abort* 'verdict-abort*))
    (t
     (let* ((type/no-expected-bit (if checkp
                                      (ecase basic-outcome
                                        (success 'result-success)
                                        (failure 'result-failure))
                                      (ecase basic-outcome
                                        (success 'verdict-success)
                                        (failure 'verdict-failure))))
            (expectedp (safe-subtypep type/no-expected-bit expected-type)))
       (if expectedp
           (ecase type/no-expected-bit
             (result-success 'expected-result-success)
             (result-failure 'expected-result-failure)
             (verdict-success 'expected-verdict-success)
             (verdict-failure 'expected-verdict-failure))
           (ecase type/no-expected-bit
             (result-success 'unexpected-result-success)
             (result-failure 'unexpected-result-failure)
             (verdict-success 'unexpected-verdict-success)
             (verdict-failure 'unexpected-verdict-failure)))))))


(defvar *skip* nil)

(define-condition skip (expected dismissal) ()
  (:documentation "RESULT-SKIP or VERDICT-SKIP."))

(defmacro with-skip ((&optional (skip t)) &body body)
  "WITH-SKIP skips checks and trials. It forces an immediate
  SKIP-TRIAL whenever a trial is started (which turns into a
  VERDICT-SKIP) and makes checks (without intervening trials, of
  course) evaluate normally but signal RESULT-SKIP. SKIP is NIL
  cancels the effect of any enclosing WITH-SKIP with SKIP true."
  `(let ((*skip* ,skip))
     ,@body))


;;;; Signalling OUTCOME

(deftype basic-outcome () '(member success failure skip abort*))

(declaim (notinline signal-outcome))
(defun signal-outcome (checkp basic-outcome initargs)
  (declare (type basic-outcome basic-outcome))
  (let ((try-id *try-id*)
        (expected-outcome *expected-outcome*)
        (outcome nil)
        (successp nil))
    (with-retry/go ()
      (restart-case
          (let ((outcome-type (determine-outcome-type checkp basic-outcome
                                                      expected-outcome)))
            (setq outcome (apply #'make-condition outcome-type initargs))
            (setq successp
                  ;; KLUDGE: AllegroCL and CCL seem to miscompile this.
                  #-(or allegro ccl) (typep outcome 'success)
                  #+(or allegro ccl) (subtypep outcome-type 'success))
            (dbg "signalling ~S" outcome)
            (funcall (if (and (not successp) checkp) 'error 'signal) outcome)
            successp)
        (:stream stream :condition condition)
        (continue ()
          :report "Return from the check."
          :test (null *record-event*)
          (dbg "Restart ~S called on ~S." 'continue outcome)
          successp)
        (record-event ()
          ;; In the debugger, what will happen is apparent in the type
          ;; of the condition. No need to say much.
          :report "Record the event and continue."
          ;; :TEST runs in the dynamic context of some INVOKE-RESTART.
          :test (and (eq *try-id* try-id) *record-event*)
          (dbg "Restart ~S called on ~S." 'record-event outcome)
          (funcall *record-event* outcome)
          successp)
        (force-expected-success ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'success t))
          :test (and (eq *try-id* try-id)
                     (not (typep outcome '(and expected success))))
          (dbg "Restart ~S called on ~S." 'force-expected-success outcome)
          (setq basic-outcome 'success
                expected-outcome t)
          (go retry))
        (force-unexpected-success ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'success nil))
          :test (and (eq *try-id* try-id)
                     (not (typep outcome '(and unexpected success))))
          (dbg "Restart ~S called on ~S." 'force-unexpected-success outcome)
          (setq basic-outcome 'success
                expected-outcome nil)
          (go retry))
        (force-expected-failure ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'failure t))
          :test (and (eq *try-id* try-id)
                     (not (typep outcome '(and expected failure))))
          (dbg "Restart ~S called on ~S." 'force-expected-failure outcome)
          (setq basic-outcome 'failure
                expected-outcome t)
          (go retry))
        (force-unexpected-failure ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'failure nil))
          :test (and (eq *try-id* try-id)
                     (not (typep outcome '(and unexpected failure))))
          (dbg "Restart ~S called on ~S." 'force-unexpected-failure outcome)
          (setq basic-outcome 'failure
                expected-outcome nil)
          (go retry))
        (abort-check ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'abort*))
          :test (and (eq *try-id* try-id) checkp
                     (not (typep outcome 'abort*)))
          (dbg "Restart ~S called on ~S." 'abort-check outcome)
          (setq basic-outcome 'abort*)
          (go retry))
        (skip-check ()
          :report (report-change-outcome-to
                   stream (determine-outcome-type checkp 'skip))
          :test (and (eq *try-id* try-id) checkp
                     (not (typep outcome 'skip)))
          (dbg "Restart ~S called on ~S." 'skip-check outcome)
          (setq basic-outcome 'skip)
          (go retry))
        (retry-check ()
          :report "Retry check."
          :test (and (eq *try-id* try-id) checkp
                     (not (typep outcome 'success)))
          (dbg "Restart ~S called on ~S." 'retry-check outcome)
          ;; KLUDGE: Without VALUES, ECL complains about "unknown
          ;; keyword :RETRY
          ;; (https://gitlab.com/embeddable-common-lisp/ecl/-/issues/666).
          (values :retry))))))

(defun report-change-outcome-to (stream outcome-type)
  (format stream "Change outcome to ~S." outcome-type))


(defsection @try/outcome-restarts (:title "Outcome Restarts")
  (force-expected-success function)
  (force-unexpected-success function)
  (force-expected-failure function)
  (force-unexpected-failure function))

(defun force-expected-success (&optional condition)
  "Change the type of the OUTCOME being signalled to EXPECTED and
  SUCCESS. If the original condition is a RESULT, then this will be
  EXPECTED-RESULT-SUCCESS, if it is a VERDICT, then
  EXPECTED-VERDICT-SUCCESS."
  (declare (ignore condition))
  (invoke-restart 'force-expected-success))

(defun force-unexpected-success (&optional condition)
  "Change the type of OUTCOME being signalled to UNEXPECTED and
  SUCCESS."
  (declare (ignore condition))
  (invoke-restart 'force-unexpected-success))

(defun force-expected-failure (&optional condition)
  "Change the type of OUTCOME being signalled to EXPECTED and
  FAILURE."
  (declare (ignore condition))
  (invoke-restart 'force-expected-failure))

(defun force-unexpected-failure (&optional condition)
  "Change the type of OUTCOME being signalled to UNEXPECTED and
  FAILURE."
  (declare (ignore condition))
  (invoke-restart 'force-unexpected-failure))


(defsection @try/checks (:title "Checks")
  "Checks are like CL:ASSERTs, they check whether some condition holds
  and signal an OUTCOME. The outcome signalled for checks is a
  subclass of RESULT.

  Take, for example, `(IS (= X 5))`. Depending on whether `X` is
  indeed 5, some kind of RESULT SUCCESS or FAILURE will be signalled.
  WITH-EXPECTED-OUTCOME determines whether it's EXPECTED or
  UNEXPECTED, and we have one of EXPECTED-RESULT-SUCCESS,
  UNEXPECTED-RESULT-SUCCESS, EXPECTED-RESULT-FAILURE,
  UNEXPECTED-RESULT-FAILURE to signal. Furthermore, if WITH-SKIP is in
  effect, then RESULT-SKIP is signalled.

  The result is signalled with `#'SIGNAL` if it is a PASS, else it's
  signalled with `#'ERROR`. This distinction matters only if the event
  is not handled, which is never the case in a TRIAL. Standalone
  checks though - those that are not enclosed by a trial - invoke the
  debugger on RESULTs which are not of type PASS.

  The signalled RESULT is not final until RECORD-EVENT is invoked on
  it, and it can be changed with the @TRY/OUTCOME-RESTARTS and the
  @TRY/CHECK-RESTARTS."
  (result condition)
  (expected-result-success condition)
  (unexpected-result-success condition)
  (expected-result-failure condition)
  (unexpected-result-failure condition)
  (result-skip condition)
  (result-abort* condition)
  (@try/check-restarts section))

(defsection @try/check-restarts (:title "Check Restarts")
  (abort-check function)
  (skip-check function)
  (retry-check function))

(defun abort-check (&optional condition)
  "Change the OUTCOME of the check being signalled to `RESULT-ABORT*`.
  `RESULT-ABORT*`, being `(NOT PASS)`, will cause the check to return
  NIL if RECORD-EVENT is invoked on it."
  (declare (ignore condition))
  (invoke-restart 'abort-check))

(defun skip-check (&optional condition)
  "Change the OUTCOME of the check being signalled to RESULT-SKIP.
  RESULT-SKIP, being a PASS, will cause the check to return T if
  CONTINUE or RECORD-EVENT is invoked on it."
  (declare (ignore condition))
  (invoke-restart 'skip-check))

(defun retry-check (&optional condition)
  "Initiate a [non-local exit][clhs] to go reevaluate the forms
  wrapped by the check without signalling an OUTCOME."
  (declare (ignore condition))
  (invoke-restart 'retry-check))

