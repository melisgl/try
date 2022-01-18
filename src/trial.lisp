(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try/trials (:title "Trials")
  (trial type)
  (current-trial function)
  (@try/trial-events section)
  (@try/trial-verdicts section)
  (@try/trial-restarts section))

(declaim (special *count*))

(defclass trial ()
  ((test-name
    :initform (error "TRIALs are not to be instantiated directly.")
    ;; We use a non-exported symbol as the initarg to force a clear
    ;; access to internals when instantiating TRIALs directly.
    :initarg %test-name :reader test-name)
   ;; CFORM (short for call-form, in honour of CLHS `function-form')
   ;; is like (<FUNCTION-DESIGNATOR> . <ARGS>), where
   ;; FUNCTION-DESIGNATOR may be a symbol denoting a global function
   ;; or a function object.
   ;;
   ;; - For trials created by calling a function defined with DEFTEST,
   ;;   this is (<NAME-OF-A-DEFTEST> . <ARGS>). The call may be
   ;;   explicitly to TRY, e.g. (TRY 'NAME-OF-A-DEFTEST), or implicit
   ;;   as in (NAME-OF-A-DEFTEST), which calls TRY implicitly if it is
   ;;   not already running within a TRY call. These trials are
   ;;   NAMED-TRIAL-P.
   ;;
   ;; - For trials created by WITH-TEST, the FUNCTION-DESIGNATOR is
   ;;   the actual function object (often a closure) corresponding to
   ;;   the body of WITH-TEST. These trials are LAMBDA-TRIAL-P and
   ;;   have no ARGS.
   ;;
   ;; - For trials created by calling TRY, this is (TRY <TESTABLE>)
   ;;   where TESTABLE is, of course, the argument that was passed to
   ;;   TRY when this trial was created. Among other things, the
   ;;   TESTABLE may be a symbol, a function, a list, or a package.
   ;;   These trials are TRY-TRIAL-P.
   (cform :initarg :cform :reader cform)
   (parent :initform *trial* :reader parent)
   ;; The length of the PARENT chain.
   (depth :initform (if *trial* (1+ (slot-value *trial* 'depth)) 0)
          :reader depth)
   (abort-restart :accessor abort-restart)
   (skip-restart :accessor skip-restart)
   (retry-restart :accessor retry-restart)
   ;; The most recent TRIAL-START event for this trial to support
   ;; delaying decisions about whether to print or collect it.
   (trial-start :initform nil :reader trial-start)
   ;; ELAPSED-SECONDS, CATEGORIES, and COUNT are for REPLAY-EVENTS.
   (elapsed-seconds :initform nil :initarg :elapsed-seconds
                    :reader elapsed-seconds)
   (categories :initform (if *trial* (categories *trial*) *categories*)
               :initarg :categories :reader categories)
   (count :initform (if *trial* (count-of *trial*) *count*)
          :initarg :count :reader count-of)
   ;; The rest of the slots are reinitialized in
   ;; START-RETRY-FOR-TRIAL.
   (n-retries
    :initform 0 :initarg :n-retries :reader n-retries
    :documentation "The number of times this [TRIAL][dislocated] has
    been retried. See RETRY-TRIAL.")
   (how-to-end :initform :return :initarg :how-to-end
               ;; NIL means the trial is finished. :ABORT, :RETRY and
               ;; :SKIP mean that the corresponding restart (e.g.
               ;; ABORT-TRIAL) has been invoked, but END-TRIAL has not
               ;; been run yet. :RETURN means that none of the above
               ;; restarts have been invoked so the trial is expected
               ;; to return normally.
               :type (member nil :return :abort :retry :skip)
               :accessor how-to-end)
   (how-it-ended :initform nil :reader how-it-ended)
   (verdict
    :initform nil :reader verdict
    :documentation "The [VERDICT][condition] EVENT signalled when this
    [TRIAL][dislocated] finished or NIL if it has not finished yet.")
   (children
    :initform () :initarg :children :reader children
    :documentation "A list of immediate child VERDICTs, RESULTs, and
    ERROR*s collected in reverse chronological order (see
    @TRY/COLLECT). The VERDICT of this TRIAL is not among CHILDREN,
    but the VERDICTs of child trials' are.")
   (has-non-collected-failed-child-p
    :initform nil
    :initarg :has-non-collected-failed-child-p
    :reader has-non-collected-failed-child-p)
   ;; See @TRY/COUNT.
   (counter :initarg :counter :reader counter)
   ;; Counts of stuff not on CHILDREN for @TRY/RERUN and @TRY/REPLAY.
   (non-collected-counter :initarg :non-collected-counter
                          :reader non-collected-counter))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation """Trials are records of calls to tests (see
  @TRY/COUNT, @TRY/COLLECT). Their behaviour as @FUNCALLABLE-INSTANCEs
  is explained in @TRY/RERUN.

  There are three ways to acquire a TRIAL object: by calling
  CURRENT-TRIAL, through the lexical binding of the symbol that names
  the test or through the return value of a test:

  ```
  (deftest xxx ()
    (prin1 xxx))

  (xxx)
  .. #<TRIAL (XXX) RUNNING>
  ==> #<TRIAL (XXX) EXPECTED-SUCCESS 0.000s>
  ```

  WITH-TRIAL can also provide access to its TRIAL:

  ```
  (with-test (t0)
    (prin1 t0))
  .. #<TRIAL (WITH-TEST (T0)) RUNNING>
  ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s>
  ```

  TRIALs are not to be instantiated by client code.
  """))

(declaim (ftype function call-trial))

(defmethod initialize-instance :after ((trial trial) &key)
  (let ((categories (categories trial)))
    (dolist (slot-name '(counter non-collected-counter))
      (unless (slot-boundp trial slot-name)
        (setf (slot-value trial slot-name) (make-counter categories)))))
  ;; Make FUNCALL work on trials.
  (closer-mop:set-funcallable-instance-function
   trial
   #-cmucl
   (lambda ()
     (call-trial trial))
   ;; CMUCL sigsegvs or produces nonsense when the instance function
   ;; is a closure.
   #+cmucl
   (eval `(lambda ()
            (call-trial ,trial)))))

(defmethod print-object ((trial trial) stream)
  (print-unreadable-object (trial stream :type t)
    (with-slots (test-name how-to-end) trial
      (format stream "~S~@[ ~A~]~@[ RETRY#~A~]~@[ ~A~]~@[ ~,3Fs~]"
              (ignore-errors
               (if (lambda-trial-p trial)
                   `(with-test (,(test-name trial)))
                   (cform trial)))
              (ignore-errors
               (case how-to-end
                 ((nil) nil)
                 (:return "RUNNING")
                 (:abort "ABORTING")
                 (:skip "SKIPPING")
                 (:retry "RETRYING")
                 (t (format nil "UNKNOWN ~S" how-to-end))))
              (ignore-errors (when (plusp (n-retries trial))
                               (n-retries trial)))
              (ignore-errors (and (verdict trial)
                                  (event-category (verdict trial)
                                                  *categories*)))
              (ignore-errors (elapsed-seconds trial))))
    (ignore-errors (write-trial-counts trial stream))))

(defun current-trial ()
  "TRIALs, like the calls to tests they stand for, nest. CURRENT-TRIAL
  returns the innermost trial. If there is no currently running test,
  then an error is signalled. The returned trial is RUNNINGP."
  (or *trial* (error "No current trial.")))

(defun trialp (object)
  (typep object 'trial))

(defun named-trial-p (trial)
  (let ((function-designator (first (cform trial))))
    (and (not (eq function-designator 'try))
         (symbolp function-designator))))

(defun lambda-trial-p (trial)
  (functionp (first (cform trial))))

(defun try-trial-p (trial)
  (eq (first (cform trial)) 'try))

(defun try-trial-testable (trial)
  (destructuring-bind (function-name &rest args) (cform trial)
    (assert (eq function-name 'try))
    (assert (= (length args) 1))
    (first args)))


(defsection @try/trial-verdicts (:title "Trial Verdicts")
  "When a trial finished, a VERDICT is signalled. The verdict's type
  is determined as follows.

  - It is a VERDICT-SKIP if

      - SKIP-TRIAL was called on the trial, or
      - ABORT-TRIAL, SKIP-TRIAL, or RETRY-TRIAL was called on an
        enclosing trial, and
      - these were not overruled by a later ABORT-TRIAL or RETRY-TRIAL
        on the trial.

  - It is a VERDICT-ABORT* if ABORT-TRIAL was called on the trial, and
    it wasn't overruled by a later SKIP-TRIAL or RETRY-TRIAL.

  - If all children (including those not collected in CHILDREN) of the
    trial PASS, then the verdict will be a SUCCESS, else it will be a
    FAILURE.

  - Subject to the WITH-EXPECTED-OUTCOME in effect,
    `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}` is the type of
    the verdict which will be signalled.

  The verdict of this type is signalled, but its type can be changed
  by the @TRY/OUTCOME-RESTARTS or the @TRY/TRIAL-RESTARTS before
  RECORD-EVENT is invoked on it."
  (verdict (reader trial))
  (runningp function)
  (passedp function)
  (failedp function))

(defun runningp (trial)
  "See if the function call associated with TRIAL has not returned yet.
  Trials that are not running have a VERDICT and are said to be
  finished."
  (null (verdict trial)))

(defun passedp (trial)
  "See if TRIAL has finished and its [VERDICT][(reader trial)] is a
  PASS."
  (and (verdict trial) (typep (verdict trial) 'pass)))

(defun failedp (trial)
  "See if TRIAL has finished and its [VERDICT][(reader trial)] is a
  FAIL."
  (and (verdict trial) (not (typep (verdict trial) 'pass))))


;;;; Signalling TRIAL-START and VERDICTs. This is the outermost layer
;;;; in WITH-TRIAL.

(declaim (ftype (function (t) t) maybe-mark-test-run))

(defun start-trial (trial)
  (maybe-mark-test-run trial)
  (signal-with-record-event (make-condition 'trial-start :trial trial)))

(defun end-trial (trial)
  (with-slots (how-to-end how-it-ended parent) trial
    (assert (not (null how-to-end)))
    (let* ((basic-outcome (%trial-basic-verdict trial))
           (previous-record-event *record-event*)
           (*record-event*
             (lambda (verdict)
               (assert (eq (trial verdict) trial))
               (setf (slot-value trial 'verdict) verdict)
               ;; No *RECORD-EVENT* during replay (see REPLAY-EVENTS),
               ;; and we don't want to overwrite ELAPSED-SECONDS in
               ;; that case.
               (when previous-record-event
                 (setf (slot-value trial 'elapsed-seconds)
                       (setf (slot-value trial 'elapsed-seconds)
                             (get-elapsed-seconds)))
                 (funcall previous-record-event verdict)))))
      ;; For REPLAY-EVENTS.
      (setf how-it-ended how-to-end)
      (setf how-to-end nil)
      (signal-outcome nil basic-outcome (list :trial trial)))))

(defun %trial-basic-verdict (trial)
  (with-slots (how-to-end has-non-collected-failed-child-p children) trial
    (cond ((eq how-to-end :skip)
           'skip)
          ((member how-to-end '(:abort :retry))
           'abort*)
          ((or has-non-collected-failed-child-p
               (some (lambda (child)
                       (typep child 'fail))
                     children))
           'failure)
          (t
           'success))))

(defun start-retry-for-trial (trial)
  (with-slots (n-retries how-to-end how-it-ended
               verdict children has-non-collected-failed-child-p
               counter non-collected-counter)
      trial
    (incf n-retries)
    (setf how-to-end :return
          how-it-ended nil
          verdict nil
          children ()
          has-non-collected-failed-child-p nil)
    (reset-counter counter)
    (reset-counter non-collected-counter))
  (assert (eq trial *trial*))
  (signal-with-record-event (make-condition 'trial-start :trial trial)))


(defsection @try/trial-restarts (:title "Trial Restarts")
  """There are three restarts available for manipulating running
  trials: ABORT-TRIAL, SKIP-TRIAL, and RETRY-TRIAL. They may be
  invoked programatically or from the debugger. ABORT-TRIAL is also
  invoked by TRY when encountering UNHANDLED-ERROR.

  The functions below invoke one of these restarts associated with a
  TRIAL. It is an error to call them on trials that are not RUNNINGP,
  but they may be called on trials other than the CURRENT-TRIAL. In
  that case, any intervening trials are skipped.

  ```
  ;; Skipped trials are marked with '-' in the output.
  (with-test (outer)
    (with-test (inner)
      (is t)
      (skip-trial nil outer)))
  .. OUTER
  ..   INNER
  ..     ⋅ (IS T)
  ..   - INNER ⋅1
  .. - OUTER ⋅1
  ..
  ==> #<TRIAL (WITH-TEST (OUTER)) SKIP 0.000s ⋅1>
  ```

  Furthermore, all three restarts initiate a @NON-LOCAL-EXIT to return
  from the trial. If during the unwinding of the stack, the
  non-local-exit is cancelled (see @CANCELLED-NLX), the appropriate
  restart will be invoked upon returning from the trial. In the
  following example, the non-local exit from a skip is cancelled by a
  THROW.

  ```
  (with-test (some-test)
    (catch 'foo
      (unwind-protect
           (skip-trial)
        (throw 'foo nil)))
    (is t :msg "check after skip"))
  .. SOME-TEST
  ..   ⋅ check after skip
  .. - SOME-TEST ⋅1
  ..
  ==> #<TRIAL (WITH-TEST (SOME-TEST)) SKIP 0.000s ⋅1>
  ```

  In the next example, the non-local exit from a skip is cancelled by
  an ERROR, which triggers an ABORT-TRIAL.

  ```
  (let ((*debug* nil)
        (*describe* nil))
    (with-test (foo)
      (unwind-protect
           (skip-trial)
        (error "xxx"))))
  .. FOO
  ..   ⊟ "xxx" (SIMPLE-ERROR)
  .. ⊟ FOO ⊟1
  ..
  ==> #<TRIAL (WITH-TEST (FOO)) ABORT* 0.000s ⊟1>
  ```

  All three restarts may be invoked on any EVENT, including the
  trial's own TRIAL-START and VERDICT. If their [CONDITION][argument]
  argument is an EVENT (RETRY-TRIAL has a special case here), they
  also record it (as in RECORD-EVENT) to ensure that when they handle
  an EVENT in the debugger or programatically that event is not
  dropped.
  """
  (abort-trial function)
  (skip-trial function)
  (retry-trial function)
  (n-retries (reader trial)))

(defun abort-trial (&optional condition (trial (current-trial)))
  "Invoke the ABORT-TRIAL restart of a RUNNINGP TRIAL.

  When CONDITION is a VERDICT for TRIAL, ABORT-TRIAL signals a new
  verdict of type VERDICT-ABORT*. This behavior is similar to that of
  ABORT-CHECK. Else, the ABORT-TRIAL restart may record CONDITION,
  then it initiates a @NON-LOCAL-EXIT to return from the test function
  with VERDICT-ABORT*. If during the unwinding SKIP-TRIAL or
  RETRY-TRIAL is called, then the abort is cancelled.

  Since ABORT* is an UNEXPECTED EVENT, ABORT-TRIAL is rarely used
  programatically. Signalling any error in a trial that's not caught
  before the trial's handler catches it will get turned into an
  UNHANDLED-ERROR, and TRY will invoke ABORT-TRIAL with it. Thus,
  instead of invoking ABORT-TRIAL directly, signalling an error will
  often suffice."
  (check-trial-running trial)
  (dbg "(~S ~S ~S)" 'abort-trial condition trial)
  (invoke-restart (abort-restart trial) condition))

(defun skip-trial (&optional condition (trial (current-trial)))
  "Invoke the SKIP-TRIAL restart of a RUNNINGP TRIAL.

  When CONDITION is a VERDICT for TRIAL, SKIP-TRIAL signals a new
  verdict of type VERDICT-SKIP. This behavior is similar to that of
  SKIP-CHECK. Else, the SKIP-TRIAL restart may record CONDITION, then
  it initiates a @NON-LOCAL-EXIT to return from the test function with
  VERDICT-SKIP. If during the unwinding ABORT-TRIAL or RETRY-TRIAL is
  called, then the skip is cancelled.

  ```
  (with-test (skipped)
    (handler-bind ((unexpected-result-failure #'skip-trial))
      (is nil)))
  .. SKIPPED
  ..   ⊠ (IS NIL)
  .. - SKIPPED ⊠1
  ..
  ==> #<TRIAL (WITH-TEST (SKIPPED)) SKIP 0.000s ⊠1>
  ```

  Invoking SKIP-TRIAL on the TRIAL's own TRIAL-START skips the trial
  being started.

  ```
  (let ((*print* '(or outcome leaf)))
    (with-test (parent)
      (handler-bind ((trial-start #'skip-trial))
        (with-test (child)
          (is nil)))))
  .. PARENT
  ..   - CHILD
  .. ⋅ PARENT
  ..
  ```
  "
  (check-trial-running trial)
  (dbg "(~S ~S)" 'skip-trial trial)
  (invoke-restart (skip-restart trial) condition))

(defun retry-trial (&optional condition (trial (current-trial)))
  "Invoke the RETRY-TRIAL restart of RUNNINGP TRIAL. The RETRY-TRIAL
  restart may record CONDITION, then it initiates a @NON-LOCAL-EXIT to
  go back to the beginning of the test function. If the non-local exit
  completes, then

  - (N-RETRIES TRIAL) is incremented,
  - collected results and trials are cleared (see @TRY/COLLECT),
  - counts are zeroed (see @TRY/COUNT), and
  - TRIAL-START is signalled again.

  If during the unwinding ABORT-TRIAL or SKIP-TRIAL is called, then
  the retry is cancelled.

  CONDITION (which may be NIL) is recorded if it is an EVENT but not
  the VERDICT of TRIAL, and the RECORD-EVENT restart is available."
  (check-trial-running trial)
  (dbg "(~S ~S)" 'retry-trial trial)
  (invoke-restart (retry-restart trial) condition))

(defun check-trial-running (trial)
  (check-type trial trial)
  (unless (runningp trial)
    (error "~S is finished." trial)))

;;; WITH-TRIAL-RESTARTS is the middle layer of WITH-TRIAL.
(defmacro with-trial-restarts ((trial &key return-form retry-form
                                resignal-verdict-form)
                               &body body)
  (assert (symbolp trial))
  (with-gensyms (try-id last-condition-tested-for stream condition)
    `(let ((,try-id *try-id*)
           (,last-condition-tested-for nil))
       (restart-bind
           ((:stream ,stream :condition ,condition)
            ;; This is what TRY invokes on an error. In case we end up
            ;; in the debugger on an unexpected error in BODY, let's
            ;; make it the first restart so it's kind of the default
            ;; suggestion.
            (abort-trial
              (,condition)
              :test (progn
                      (when ,condition
                        (setq ,last-condition-tested-for ,condition))
                      #+nil
                      (dbg "ABORT-TRIAL testing ~S" ,condition)
                      (and (eq *try-id* ,try-id)
                           (not (and (verdict-for-trial-p ,trial ,condition)
                                     (typep ,condition 'abort*)))))
              :report (if (verdict-for-trial-p ,trial
                                               ,last-condition-tested-for)
                          (report-change-outcome-to ,stream 'abort*)
                          (format ,stream
                                  "~:[Abort~;Record the event and abort~] ~
                                   trial ~S."
                                  (typep ,last-condition-tested-for 'event)
                                  (test-name ,trial)))
              ;; This is the debugger's condition pilfered by :TEST.
              :interactive (list ,last-condition-tested-for)
              (dbg "Restart ~S called in ~S with ~S."
                   'abort-trial ,trial ,condition)
              (say-how-to-end ,trial :abort :skip)
              (cond ((verdict-for-trial-p ,trial ,condition)
                     ,resignal-verdict-form)
                    (t
                     (when (typep ,condition 'event)
                       (funcall *record-event* ,condition))
                     ,return-form)))
            (skip-trial
              (,condition)
              :test (and (eq *try-id* ,try-id)
                         (not (and (verdict-for-trial-p ,trial ,condition)
                                   (typep ,condition 'skip))))
              :report (if (verdict-for-trial-p ,trial
                                               ,last-condition-tested-for)
                          (report-change-outcome-to ,stream 'skip*)
                          (format ,stream
                                  "~:[Skip~;Record the event and skip~] ~
                                   trial ~S."
                                  (typep ,last-condition-tested-for 'event)
                                  (test-name ,trial)))
              :interactive (list ,last-condition-tested-for)
              (dbg "Restart ~S called on ~S." 'skip-trial ,trial)
              (say-how-to-end ,trial :skip :skip)
              (cond ((verdict-for-trial-p ,trial ,condition)
                     ,resignal-verdict-form)
                    (t
                     (when (typep ,condition 'event)
                       (funcall *record-event* ,condition))
                     ,return-form)))
            (retry-trial
              (,condition)
              :test (eq *try-id* ,try-id)
              :report (format ,stream
                              "~:[Retry~;Record the event and retry~] ~
                               trial ~S."
                              (and (typep ,last-condition-tested-for 'event)
                                   (not (verdict-for-trial-p
                                         ,trial ,last-condition-tested-for)))
                              (test-name ,trial))
              :interactive (list ,last-condition-tested-for)
              (dbg "Restart ~S called in ~S with ~S."
                   'retry-trial ,trial ,condition)
              (when (and (typep ,condition 'event)
                         (not (verdict-for-trial-p ,trial ,condition)))
                (funcall *record-event* ,condition))
              (say-how-to-end ,trial :retry :skip)
              ,retry-form))
         (set-up-trial-restarts ,trial)
         (%unwind-protect
             (progn ,@body)
           ;; The restart objects are dynamic extent. Don't hold on to
           ;; them.
           (clear-trial-restarts ,trial))))))

(defun verdict-for-trial-p (trial event)
  (and (typep event 'verdict)
       (eq (trial event) trial)))

(defun say-how-to-end (ancestor-trial ancestor-how-to-end
                       until-then-how-to-end)
  (assert *trial*)
  (loop for trial = *trial* then (parent trial)
        until (eq trial ancestor-trial)
        do (assert trial)
           (setf (how-to-end trial) until-then-how-to-end)
        finally (when ancestor-how-to-end
                  (setf (how-to-end trial) ancestor-how-to-end))))

(defun set-up-trial-restarts (trial)
  (setf (abort-restart trial) (find-restart 'abort-trial)
        (skip-restart trial) (find-restart 'skip-trial)
        (retry-restart trial) (find-restart 'retry-trial)))

(defun clear-trial-restarts (trial)
  (setf (abort-restart trial) nil
        (skip-restart trial) nil
        (retry-restart trial) nil))


;;;; Handling of unhandled errors and non-local exits. The innermost
;;;; layer in WITH-TRIAL.

;;; Handle SERIOUS-CONDITIONs by resignalling them as
;;; UNHANDLED-ERRORs, and signal an NLX (both ERROR*s) if BODY does
;;; not return normally although HOW-TO-END is :RETURN.
(defmacro with-error*-handled ((trial) &body body)
  (assert (symbolp trial))
  (with-gensyms (condition)
    `(on-nlx
         (handler-bind (((and serious-condition (not try-internal-error))
                          (lambda (,condition)
                            (signal-error* ,condition ,trial))))
           (with-debugger-hook (lambda (,condition)
                                 (unless (typep ,condition 'event)
                                   (signal-error* ,condition ,trial
                                                  :debugger-invoked-p t)))
             ,@body))
       (when (eq (how-to-end ,trial) :return)
         (signal-error* nil ,trial)))))

;;; Retry is handled in between START-TRIAL and END-TRIAL, so normally
;;; we shouldn't see HOW-TO-END :RETRY in END-TRIAL. Also, when
;;; retrying an ancestor trial, the intervening trials are :SKIPped.
;;; Tested by TRY-TEST::TEST-WITH-TEST/CANCEL-NLX-OF-RETRY/OUTSIDE.
(defun clean-up-cancelled-nlx-of-retry-for-end-trial (trial)
  (when (eq (how-to-end trial) :retry)
    (setf (how-to-end trial) :return)
    (assert (eq *trial* trial))
    (signal-error* nil trial
                   :extra-message
                   (format nil " (which cancelled the nlx of ~S)"
                           'retry-trial))))


;;; This macro is the heart of the library. It is a ball of non-local
;;; transfer of control and lot of worrying about how they might be
;;; cancelled.
(defmacro with-trial ((trial) &body body)
  (assert (symbolp trial))
  (with-gensyms (return-from-trial-catch retry-catch resignal-verdict-catch
                                         values)
    `(let (;; These must be unique per invocation.
           (,return-from-trial-catch (make-gensym '#:return-from-trial))
           (,retry-catch (make-gensym '#:retry-catch))
           (,resignal-verdict-catch (make-gensym '#:resignal-verdict)))
       (with-timing
         (let ((*trial* ,trial))
           ;; This CATCH is where the ABORT-TRIAL and SKIP-TRIAL
           ;; restarts THROW to return from TRIAL normally. Throwing
           ;; starts the unwinding process from the depths of BODY
           ;; (the restarts are in a RESTART-BIND). Before we get
           ;; here, their nlx may be cancelled, e.g. by invoking
           ;; SKIP-TRIAL while unwinding from ABORT-TRIAL. Tested by
           ;; TRY-TEST::TEST-WITH-TEST/NLX.
           (catch ,return-from-trial-catch
             (with-trial-restarts
                 (,trial
                  :return-form (throw ,return-from-trial-catch ,trial)
                  :retry-form (throw ,retry-catch nil)
                  :resignal-verdict-form (throw ,resignal-verdict-catch nil))
               (with-retry/catch (:catch ,retry-catch
                                   :on-retry (start-retry-for-trial
                                              ,trial))
                 (unwind-protect
                      (with-retry/catch (:catch ,retry-catch
                                          :on-retry (start-retry-for-trial
                                                     ,trial))
                        (start-trial ,trial)
                        (when *skip*
                          (skip-trial))
                        (let ((,values (multiple-value-list
                                        (with-error*-handled (,trial)
                                          ,@body))))
                          (when (eq (how-to-end ,trial) :retry)
                            ;; When TRIAL was marked for retry,
                            ;; (GO-RETRY) initiated an nlx, which must
                            ;; have been cancelled since we are
                            ;; returning normally. Do retry.
                            (retry-trial ,trial))
                          (values-list (cons ,trial ,values))))
                   ;; This cleanup must be outside the inner
                   ;; WITH-RETRY to detect if a RETRY-TRIAL's nlx was
                   ;; cancelled by another nlx going further out. But
                   ;; both the cleanup and END-TRIAL must be inside
                   ;; another WITH-RETRY so that RETRY-TRIAL in
                   ;; response to the conditions signalled works.
                   ;; Without intervening user code, there is no way
                   ;; for an nlx to the outher target to be cancelled.
                   (clean-up-cancelled-nlx-of-retry-for-end-trial ,trial)
                   (with-retry/catch (:catch ,resignal-verdict-catch)
                     (end-trial ,trial)))))))))))
