(in-package :try)

(defsection @try/rerun (:title "Rerunning Trials")
  "When a TRIAL is `FUNCALL`ed or passed to TRY, the _test that
  created the trial_ is invoked, and it may be run again in its
  entirety or in part. As the test runs, it may invoke other tests.
  Any test (including the top-level one) is skipped if it does not
  correspond to a [collected][@try/collect] trial or its TRIAL-START
  event and VERDICT do not match the RERUN argument of TRY. When that
  happens, the corresponding function call immediately returns the
  TRIAL object.

  - A new trial is skipped (as if with SKIP-TRIAL) if RERUN is not T
    and

      - there is no trial representing the same function call among
        the collected but not yet rerun trials in the trial being
        rerun, or

      - the first such trial does not match the RERUN type argument of
        TRY in that neither its TRIAL-START, VERDICT events match the
        type RERUN, nor do any of its collected RESULTs and trials.

  - The _test that created the trial_ is determined as follows.

      - If the trial was created by calling a DEFTEST function, then
        the test currently associated with that symbol naming the
        function is called with the arguments of the original function
        call. If the symbol is no longer FBOUNDP (because it was
        FMAKUNBOUND) or it no longer names a DEFTEST (it was redefined
        with DEFUN), then an error is signalled.

      - If the trial was created by entering a WITH-TEST form, then
        its body is executed again in the original lexical but the
        current dynamic environment. Implementationally speaking,
        WITH-TEST defines a local function of no arguments (likely a
        closure) that wraps its body, stores the closure in the trial
        object and calls it on a rerun in a WITH-TEST of the same
        TRIAL-VAR and same NAME.

      - If the trial was created by TRY itself to ensure that all
        events are signalled in a trial (see @TRY/EXPLICIT-TRY), then
        on a rerun the same TESTABLE is run again.

      All three possibilities involve entering DEFTEST or WITH-TEST, or
      invoking TRY: the same cases that we have when calling tests
      functions (see @TRY/IMPLICIT-TRY). Thus, even if a trial is rerun
      with FUNCALL, execution is guaranteed to happen under TRY.")

(declaim (ftype (function (t) t) try/implicit))

(defun call-trial (trial)
  (if *try-id*
      (let* ((cform (cform trial))
             (function-designator (first cform)))
        (cond ((symbolp function-designator)
               (call-named-trial cform))
              ((functionp function-designator)
               (call-lambda-trial trial))
              (t
               (call-try-trial trial))))
      (try/implicit trial)))

(defun call-named-trial (cform)
  (destructuring-bind (symbol &rest args) cform
    (assert (symbolp symbol))
    (unless (fboundp symbol)
      (error "~@<Cannot call ~S because it is no longer ~S.~:@>"
             symbol 'fboundp))
    (unless (test-bound-p symbol)
      (error "~@<Cannot call ~S because it is no longer ~S.~:@>"
             symbol 'test-bound-p))
    (apply symbol args)))

(defun call-lambda-trial (trial)
  (let ((cform (cform trial)))
    (destructuring-bind (function &rest args) cform
      (assert (functionp function))
      (assert (endp args))
      (let ((trial (make-instance 'trial
                                  '%test-name (test-name trial)
                                  :cform cform)))
        (with-trial (trial)
          (funcall function trial))))))

(defun call-try-trial (trial)
  (let ((cform (cform trial)))
    (destructuring-bind (function-designator &rest args) cform
      (assert (eq function-designator 'try))
      (destructuring-bind (testable) args
        (call-testable testable)))))


(defun %make-rerun-trial-event-handler (trial-to-rerun rerun-type)
  (let ((trial-being-rerun nil)
        ;; The first element of CHILD-TRIALS-NOT-RUN is a list of
        ;; children of TRIAL-BEING-RERUN which have not been skipped
        ;; nor run, yet.
        (child-trials-not-run (list (list trial-to-rerun)))
        (skippingp nil))
    (labels ((mark-child-trial-run (trial)
               (setq child-trials-not-run
                     (cons (remove trial (first child-trials-not-run))
                           (rest child-trials-not-run))))
             (enter-trial (trial)
               (mark-child-trial-run trial)
               (setq trial-being-rerun trial)
               (push (reverse (child-trials trial)) child-trials-not-run))
             (leave-trial ()
               (pop child-trials-not-run)
               (setq trial-being-rerun (parent trial-being-rerun)))
             (first-child-with-same-call (trial)
               (find-if (lambda (child)
                          (trial-call-= child trial))
                        (first child-trials-not-run)))
             (rerun-trial-p (trial)
               (dbg "rerun-trial-p: ~S ~S~%" trial
                    (trial-has-event-of-type-p trial rerun-type))
               (trial-has-event-of-type-p trial rerun-type)))
      (lambda (c)
        (etypecase c
          (trial-start
           (assert (not skippingp))
           (let ((trial (trial c)))
             ;; If N-RETRIES > 0, then we have already decided when
             ;; TRIAL-START with N-RETRIES = 0 was handled here that
             ;; this trial shall be rerun.
             (when (zerop (n-retries trial))
               (let ((child-with-same-call
                       (first-child-with-same-call trial)))
                 (cond ((and child-with-same-call
                             (rerun-trial-p child-with-same-call))
                        (enter-trial child-with-same-call))
                       (t
                        (when child-with-same-call
                          (mark-child-trial-run child-with-same-call))
                        (setq skippingp t)
                        (skip-trial c)))))))
          (verdict
           (if skippingp
               (setq skippingp nil)
               (leave-trial))))))))

(defun child-trials (trial)
  (loop for child in (children trial)
        when (typep child 'verdict)
          collect (trial child)))

;;; Ignoring the dynamic environment, do these trials call the same
;;; function?
(defun trial-call-= (trial1 trial2)
  (destructuring-bind (function-designator-1 &rest args1) (cform trial1)
    (destructuring-bind (function-designator-2 &rest args2) (cform trial2)
      (or (and (eq function-designator-1 function-designator-2)
               (equal args1 args2))
          ;; Heuristically accept WITH-TESTs as the same if they have
          ;; the same name.
          (and (lambda-trial-p trial1)
               (lambda-trial-p trial2)
               (equal (test-name trial1) (test-name trial2)))))))

(defun trial-has-event-of-type-p (trial type)
  (or (safe-typep (trial-start trial) type)
      (safe-typep (verdict trial) type)
      (find-if (lambda (child)
                 (if (typep child 'verdict)
                     (trial-has-event-of-type-p (trial child) type)
                     (safe-typep child type)))
               (children trial))))
