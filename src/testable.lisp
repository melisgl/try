(in-package :try)

(defsection @try/testables (:title "Testables")
  "Valid first arguments to TRY are called testables. A testable may
  be:

  - a [function designator][clhs]

      - the name of a global test
      - the name of a global function
      - a function object
      - a trial
  - a list of testables
  - a PACKAGE

  In the function designator cases, TRY calls the designated function.
  [TRIAL][class]s, being @FUNCALLABLE-INSTANCEs, designate themselves.
  If the trial is not RUNNINGP, then it will be rerun (see
  @TRY/RERUN). Don't invoke TRY with RUNNINGP trials (but see
  @TRY/IMPLICIT-TRY-IMPLEMENTATION for discussion).

  When given a list of testables, TRY calls each testable one by one.

  Finally, a PACKAGE stands for the result of calling
  LIST-PACKAGE-TESTS on that package.")

(defun call-testable (testable)
  (multiple-value-bind (function-designators wrapper-cform)
      (list-function-designators testable)
    (if wrapper-cform
        (call-with-wrapper function-designators wrapper-cform)
        (destructuring-bind (function-designator) function-designators
          (funcall function-designator)))))

(defun call-with-wrapper (function-designators wrapper-cform)
  (let ((wrapper (make-instance 'trial
                                '%test-name wrapper-cform
                                :cform wrapper-cform)))
    (with-trial (wrapper)
      (wrap-trial-body-for-return nil
        (mapc #'funcall function-designators)))))

;;; Return two values:
;;;
;;; 1. A list of FUNCTION-DESIGNATORs to be called with no arguments
;;;    for this TESTABLE. For example, if TESTABLE is a package, then
;;;    it is the list of symbols in that package with DEFTEST
;;;    definitions. If TESTABLE is a FUNCTION-DESIGNATOR (including
;;;    TRIALs), then it is returned as (LIST TESTABLE).
;;;
;;; 2. A CFORM for an extra trial to wrap around the calls to the
;;;    function designators in the first value to ensure that all
;;;    events are produced within a trial. If no wrapping is required,
;;;    then this is NIL. When CFORM is executed, it must rerun the
;;;    equivalent of (TRY TESTABLE), hence in most cases that's
;;;    exactly what's returned.
(defun list-function-designators (testable)
  (cond ((null testable)
         ;; Do nothing in an extra trial.
         (values () `(try ())))
        ((and (symbolp testable) (test-bound-p testable))
         ;; DEFTEST establishes a trial. No need for wrapping.
         (values `(,testable) nil))
        ;; We can't return a TRY-TRIAL as the function-designator
        ;; because when funcalled it would lead us back here and to
        ;; infinite recursion.
        ((trialp testable)
         (if (try-trial-p testable)
             (let ((previous-testable (try-trial-testable testable)))
               (assert (not (and (trialp previous-testable)
                                 (try-trial-p previous-testable))))
               (values (list-function-designators previous-testable)
                       `(try ,previous-testable)))
             ;; Named and lambda trials
             (values `(,testable) nil)))
        ((or (and (symbolp testable) (fboundp testable))
             ;; TRIALs are funcallable thus FUNCTIONP so except for
             ;; TRY-TRIALs handled above) trials end up here.
             (functionp testable))
         (values (list testable) `(try ,testable)))
        ((symbolp testable)
         (error "~S is not testable because it is not ~S." testable 'fboundp))
        ((listp testable)
         (values (mapcan #'list-function-designators testable)
                 `(try ,testable)))
        ((packagep testable)
         (values (list-package-tests testable)
                 `(try ,testable)))
        (t
         (error "~S is not testable." testable))))
