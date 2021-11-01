(in-package :try)

(defsection @try/errors (:title "Errors")
  (error* condition)
  (test-name (reader error*))
  (unhandled-error condition)
  (nested-condition (reader unhandled-error))
  (backtrace-of (reader unhandled-error))
  (debugger-invoked-p (reader unhandled-error))
  (*gather-backtrace* variable)
  (nlx condition))

(define-condition error* (abort* trial-event leaf)
  ((test-name :initarg :test-name :reader test-name))
  (:documentation "Either UNHANDLED-ERROR or NLX, ERROR* causes or
  represents abnormal termination of a TRIAL. ABORT-TRIAL can be
  called with ERROR*s, but there is little need for explicitly doing
  so as RECORD-EVENT, which TRY invokes, takes care of this."))

(define-condition unhandled-error (error*)
  ((nested-condition
    :initform nil :initarg :condition :reader nested-condition
    :documentation "The unhandled condition.")
   (backtrace
    :initform nil :initarg :backtrace :reader backtrace-of
    :documentation "A string if *GATHER-BACKTRACE* was true when this
    event was signalled, NIL otherwise.")
   (debugger-invoked-p
    :initform nil :initarg :debugger-invoked-p
    :reader debugger-invoked-p
    :documentation "True if NESTED-CONDITION was caught by a trial's
    *DEBUGGER-HOOK*, NIL if it was caught by its ERROR handler."))
  (:documentation "Signalled when an CL:ERROR condition reaches the
  handlers set up DEFTEST or WITH-TEST, or when their *DEBUGGER-HOOK*
  is invoked with a condition that's not an EVENT."))

(define-condition nlx (error*)
  ((extra-message :initarg :extra-message :reader extra-message))
  (:documentation "Representing a @NON-LOCAL-EXIT of unknown origin,
  this is signalled if a TRIAL does not return normally although it
  should have because it was not dismissed (see DISMISSAL, SKIP-TRIAL,
  ABORT-TRIAL). In this case, there is no CL:ERROR associated with the
  event."))

(defvar *print-backtrace* nil)

(defmethod write-event ((unhandled-error unhandled-error) stream
                        &key terse ctx)
  (let ((c (nested-condition unhandled-error)))
    (pprint-logical-block (stream nil :per-line-prefix "")
      (cond (terse
             (when (debugger-invoked-p unhandled-error)
               (format stream "debugger invoked on "))
             (describe-condition c stream))
            (t
             (if (debugger-invoked-p unhandled-error)
                 (format stream "~@<Trial~@[ ~S~] entered the debugger ~
                                 on a condition of type ~S:~:@>~:@_"
                         (test-name unhandled-error) (type-of c))
                 (format stream "~@<Trial~@[ ~S~] encountered an ~
                                 unhandled error of type ~S:~:@>~:@_"
                         (test-name unhandled-error) (type-of c)))
             (pprint-logical-block (stream nil :per-line-prefix "  ")
               (format stream "~A" c))))
      (when (and ctx *print-backtrace* (backtrace-of unhandled-error))
        (format stream "~:@_~A" (backtrace-of unhandled-error))))))

(defun describe-condition (condition &optional stream)
  (format stream "\"~A\" (~S)" condition (type-of condition)))

(defmethod write-event ((nlx nlx) stream &key terse ctx)
  (declare (ignore ctx))
  (pprint-logical-block (stream nil :per-line-prefix "")
    (if terse
        (format stream "~@<non-local exit~@[~A~]~:@>"
                (ignore-errors (extra-message nlx)))
        (format stream "~@<Trial~@[ ~S~] encountered a ~
                        non-local exit~@[~A~].~:@>"
                (ignore-errors (test-name nlx))
                (ignore-errors (extra-message nlx))))))


;;;; Signalling ERROR*

(deftype basic-error*-outcome () '(member nlx unhandled-error))
(declaim (ftype (function (&optional t t) t) abort-trial))

(defun signal-error* (unhandled-error trial &key extra-message
                      debugger-invoked-p)
  (declare (type (or condition null) unhandled-error))
  (let ((error* nil)
        (try-id *try-id*)
        ;; NLX is signalled in the trial cleanup. Don't signal the
        ;; condition with #'ERROR, there is not a lot to be done at
        ;; that point and it would be annoying when aborting to the
        ;; toplevel. TRY will handle the condition anyway.
        (signalp (null unhandled-error)))
    (restart-case
        (progn
          (setq error*
                (if unhandled-error
                    (make-condition 'unhandled-error
                                    :test-name (test-name trial)
                                    :condition unhandled-error
                                    :backtrace (maybe-backtrace
                                                unhandled-error)
                                    :debugger-invoked-p debugger-invoked-p)
                    (make-condition 'nlx
                                    :test-name (test-name trial)
                                    :extra-message extra-message)))
          (dbg "signalling ~S" error*)
          (funcall (if signalp 'signal 'error) error*))
      (:stream stream :condition condition)
      (record-event ()
        :report (format stream "Record the event, abort trial ~S ~
                                  and continue unwinding."
                        (test-name trial))
        :test (eq *try-id* try-id)
        (dbg "Restart ~S invoked on ~S." 'continue error*)
        (cond ((typep error* 'nlx)
               (funcall *record-event* error*)
               (setf (slot-value *trial* 'how-to-end) :abort))
              (t
               (abort-trial error*)))))))

(defvar *gather-backtrace* t
  "Capturing the backtrace can be expensive. *GATHER-BACKTRACE*
  controls whether UNHANDLED-ERRORs shall have their BACKTRACE-OF
  populated.")

(defun maybe-backtrace (condition)
  ;; The backtrace for NLX (i.e. when CONDITION is NIL) is
  ;; uninformative as it is only signalled in the cleanup of the test.
  (when (and condition *gather-backtrace*)
    (with-output-to-string (s)
      (uiop/image:print-backtrace
       :stream s :condition condition))))
