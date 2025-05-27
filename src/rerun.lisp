(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @rerun (:title "Rerunning Trials")
  "When a TRIAL is `FUNCALL`ed or passed to TRY, the _test that
  created the trial_ is invoked, and it may be run again in its
  entirety or in part. As the test runs, it may invoke other tests.
  Any test (including the top-level one) is skipped if it does not
  correspond to a [collected][@collect] trial or its TRIAL-START event
  and VERDICT do not match the RERUN argument of TRY. When that
  happens, the corresponding function call immediately returns the
  TRIAL object. In trials that are rerun, @CHECKS are executed
  normally.

  - A new trial is skipped (as if with SKIP-TRIAL) if RERUN is not T
    and

      - there is no trial representing the same function call among
        the collected but not yet rerun trials in the trial being
        rerun, or

      - the first such trial does not match the RERUN type argument of
        TRY in that neither its TRIAL-START, VERDICT events match the
        type RERUN, nor do any of its collected RESULTs and trials.

  - If RERUN is T, then the test is run in its entirety, including
    even the non-collected trials. Use RERUN EVENT to run only the
    collected trials.

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
        events are signalled in a trial (see @EXPLICIT-TRY), then
        on a rerun the same TESTABLE is run again.

      All three possibilities involve entering DEFTEST or WITH-TEST,
      or invoking TRY: the same cases that we have with @IMPLICIT-TRY.
      Thus, even if a trial is rerun with FUNCALL, execution is
      guaranteed to happen under TRY."
  (*rerun-context* (variable nil)))

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

;;; FIXME: This is never called.
(defun call-try-trial (trial)
  (let ((cform (cform trial)))
    (destructuring-bind (function-designator &rest args) cform
      (assert (eq function-designator 'try))
      (destructuring-bind (testable) args
        (call-testable testable)))))


(defun %make-rerun-trial-event-handler (trial-to-rerun rerun-type)
  (let ((trial-being-rerun nil)
        ;; The first element of CHILD-TRIALS-NOT-RUN is a list of
        ;; child trials of TRIAL-BEING-RERUN which have not been
        ;; skipped nor run, yet. The rest are for the enclosing
        ;; trials.
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
             ;; Like ENTER-TRIAL, but there is no rerun event matching
             ;; for TRIAL or its descendants
             (rerun-entirely (trial)
               (setq trial-being-rerun trial)
               (push :rerun-entirely child-trials-not-run))
             ;; This pairs with both ENTER-TRIAL and RERUN-ENTIRELY.
             (leave-trial ()
               (pop child-trials-not-run)
               (setq trial-being-rerun (parent trial-being-rerun)))
             (first-child-with-same-call (trial)
               (find-if (lambda (child)
                          (trial-call-= child trial))
                        (first child-trials-not-run)))
             (rerun-trial-p (trial)
               (dbg "rerun-trial-p: ~S => ~S" trial
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
               (if (eq (first child-trials-not-run) :rerun-entirely)
                   ;; This is a descendent of RERUN-ENTIRELY-P trial.
                   (rerun-entirely trial)
                   (let ((child-with-same-call
                           (first-child-with-same-call trial)))
                     (cond ((and child-with-same-call
                                 (rerun-entirely-p child-with-same-call))
                            (rerun-entirely child-with-same-call))
                           ((and child-with-same-call
                                 (rerun-trial-p child-with-same-call))
                            (enter-trial child-with-same-call))
                           (t
                            (when child-with-same-call
                              (mark-child-trial-run child-with-same-call))
                            (setq skippingp t)
                            (skip-trial c))))))))
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


;;; FIXME: What about non-global tests?
(defvar *rerun-context* nil
  """A [TRIAL][class] or NIL. If it's a TRIAL, then TRY will
  [rerun][@rerun] this trial skipping everything that does not lead to
  an invocation of its TESTABLE argument (see @TESTABLES). If no route
  to the basic testable functions can be found among the
  [collected][@collect] events of the context, then a warning is
  signalled and the context is ignored.

  Consider the following code evaluated in the package TRY:

  ```cl-transcript (:dynenv try-transcript)
  (deftest test-try ()
    (let ((*package* (find-package :cl-user)))
      (test-whatever)
      (test-printing)))

  (deftest test-whatever ()
    (is t))

  (deftest test-printing ()
    (is (equal (prin1-to-string 'x) "TRY::X")))

  (test-try)
  .. TEST-TRY
  ..   TEST-WHATEVER
  ..     ⋅ (IS T)
  ..   ⋅ TEST-WHATEVER ⋅1
  ..   TEST-PRINTING
  ..     ⋅ (IS (EQUAL (PRIN1-TO-STRING 'X) "TRY::X"))
  ..   ⋅ TEST-PRINTING ⋅1
  .. ⋅ TEST-TRY ⋅2
  ..
  ==> #<TRIAL (TEST-TRY) EXPECTED-SUCCESS 0.500s ⋅2>

  ;; This could also be an implicit try such as (TEST-PRINTING),
  ;; but this way we avoid entering the debugger.
  (try 'test-printing)
  .. TEST-PRINTING
  ..   ⊠ (IS (EQUAL #1=(PRIN1-TO-STRING 'X) "TRY::X"))
  ..     where
  ..       #1# = "X"
  .. ⊠ TEST-PRINTING ⊠1
  ..
  ==> #<TRIAL (TEST-PRINTING) UNEXPECTED-FAILURE 0.200s ⊠1>
  ```

  TEST-PRINTING fails because when called directly, *PACKAGE* is not
  the expected `CL-USER`. However, when *RERUN-CONTEXT* is set,
  `TEST-PRINTING` will be executed in the correct dynamic environment.

  ```cl-transcript (:dynenv try-transcript)
  (setq *rerun-context*
        ;; Avoid the debugger, as a matter of style.
        (try 'test-try))
  
  (test-printing)
  .. TEST-TRY
  ..   - TEST-WHATEVER
  ..   TEST-PRINTING
  ..     ⋅ (IS (EQUAL (PRIN1-TO-STRING 'X) "TRY::X"))
  ..   ⋅ TEST-PRINTING ⋅1
  .. ⋅ TEST-TRY ⋅1
  ..
  ```

  Note how `TEST-WHATEVER` was SKIPped because it leads to no calls to
  `TEST-PRINTING`.

  See @EMACS for a convenient way of taking advantage of this feature.""")

(declaim (ftype function replay-events))

(defun filter-rerun-context-events (trial names)
  (let ((foundp nil))
    (values
     (replay-events trial
                    :collect (lambda (event)
                               (when (and (typep event 'trial-start)
                                          (find (test-name (trial event))
                                                names :test #'equal))
                                 (setq foundp t)
                                 (setf (rerun-entirely-p (trial event)) t)))
                    :print nil)
     foundp)))

(defun munge-try-args-for-rerun-context (testable rerun)
  (alexandria:nth-value-or 1
    (when *rerun-context*
      (multiple-value-bind (context foundp)
          (filter-rerun-context-events
           *rerun-context* (mapcar #'trial-to-test-name
                                   (list-function-designators testable)))
        (if foundp
            ;; We want to run everything that was not filtered out,
            ;; hence the setting of RERUN to EVENT.
            (values context 'event)
            (warn "~@<Ignoring ~S ~S as none of ~S ~S are found ~
                  among its ~Sed trials.~:@>~%"
                  '*rerun-context* *rerun-context* '@testables
                  testable '@collect))))
    (values testable rerun)))

(defun trial-to-test-name (obj)
  (if (typep obj 'trial)
      (test-name obj)
      obj))
