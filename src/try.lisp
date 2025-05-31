(in-package :try)

(defsection @calling-test-functions (:title "Calling Test Functions")
  "Tests always run under TRY, but TRY may be explicit or implicit
  depending whether the outermost test was called via TRY or directly
  as a Lisp function.

  Nested invocations of tests, be them explicit or implicit, do not
  establish nested TRYs. EVENT handling is performed only at the
  outermost level."
  (@tryvar glossary-term)
  (@explicit-try section)
  (@implicit-try section))

(defsection @implicit-try (:title "Implicit TRY")
  "We speak of an implicit TRY when the outermost test is entered
  via a Lisp function call. In this case, as the test function is
  entered, it invokes itself behind the scenes (implicitly) via TRY:

  ```
  (try ... :debug *debug* :collect *collect* :rerun *rerun*
       :print *print* :describe *describe*
       :stream *stream* :printer *printer*)
  ```

  As its invoked again, it sees that it is now running under TRY and
  proceeds to execute normally.

  An implicit TRY can only happen with the following two constructs.

  - Global test function

      ```cl-transcript (:dynenv try-transcript)
      (deftest my-test ()
        (is t))

      (my-test)
      .. MY-TEST
      ..   ⋅ (IS T)
      .. ⋅ MY-TEST ⋅1
      ..
      ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.004s ⋅1>
      ```

  - WITH-TEST

      ```cl-transcript (:dynenv try-transcript)
      (with-test (my-test)
        (is t))
      .. MY-TEST
      ..   ⋅ (IS T)
      .. ⋅ MY-TEST ⋅1
      ..
      ==> #<TRIAL (WITH-TEST (MY-TEST)) EXPECTED-SUCCESS 0.000s ⋅1>
      ```"
  (*debug* variable)
  (*count* variable)
  (*collect* variable)
  (*rerun* variable)
  (*print* variable)
  (*describe* variable)
  (*stream* (variable (make-synonym-stream '*debug-io*)))
  (*printer* variable)
  (@implicit-try-implementation section))

(define-try-var *debug* '(and unexpected (not nlx) (not verdict))
  "The default value makes TRY invoke the debugger on UNHANDLED-ERROR,
  RESULT-ABORT*, UNEXPECTED-RESULT-FAILURE, and
  UNEXPECTED-RESULT-SUCCESS. NLX is excluded because it is caught as
  the test function is being exited, but by that time the dynamic
  environment of the actual cause is likely gone. VERDICT is excluded
  because it is a consequence of its child outcomes.")

(define-try-var *count* 'leaf
  "Although the default value of *CATEGORIES* lumps RESULTs and
  VERDICTs together, with the default of LEAF, VERDICTs are not
  counted. See @COUNT.")

(define-try-var *collect* '(or trial-event unexpected)
  "By default all [TRIALs][class] and UNEXPECTED are
  [collected][@collect]. This is sufficient for being able to @RERUN
  anything in [context][*rerun-context*].")

(define-try-var *rerun* 'unexpected
  "The default matches that of *COLLECT*. See @RERUN.")

(define-try-var *print* '(or leaf dismissal)
  "Events of this type are [printed][@print].

  ```cl-transcript
  (concrete-events-of-type '(or leaf dismissal))
  => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
      EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE RESULT-SKIP
      RESULT-ABORT* VERDICT-SKIP VERDICT-ABORT* UNHANDLED-ERROR NLX)
  ```")

(define-try-var *describe* '(or unexpected failure)
  "By default, the context (e.g. @CAPTURES, and the CTX argument of
  is and other checks) of UNEXPECTED events is described. See @PRINT.")

(define-try-var *stream* (make-synonym-stream '*debug-io*))

(define-try-var *printer* 'tree-printer)

;;; CALL-TRIAL goes through here.
(defun try/implicit (trial)
  (try trial :debug *debug* :count *count* :collect *collect* :rerun *rerun*
       :print *print* :describe *describe*
       :stream *stream* :printer *printer*))

(defsection @implicit-try-implementation
    (:title "Implementation of Implicit TRY")
  "What's happening in the implementation is that a test function,
  when it is called, checks whether it is running under the TRY
  function. If it isn't, then it invokes TRY with its TRIAL. TRY
  realizes the trial cannot be rerun yet (see @RERUN) because it
  is RUNNINGP, sets up its event handlers for debugging, collecting,
  printing, and invokes the trial as if it were rerun but without
  skipping anything based on the RERUN argument. Thus the following
  are infinite recursions:

  ```
  (with-test (recurse)
    (try recurse))

  (with-test (recurse)
    (funcall recurse))
  ```")


(defsection @explicit-try (:title "Explicit TRY")
  "We speak of an explicit TRY when the outermost test function is
  called directly by TRY.

  Global test functions can be TRYed explicitly by giving their name:

  ```cl-transcript (:dynenv try-transcript)
  (deftest my-test ()
    (is t))

  (try 'my-test)
  .. MY-TEST
  ..   ⋅ (IS T)
  .. ⋅ MY-TEST ⋅1
  ..
  ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s ⋅1>
  ```

  However, WITH-TEST has no global name, so to delay its execution
  until TRY calls it, it needs to be wrapped in a LAMBDA.

  ```
  (try (lambda ()
         (with-test (my-test)
           (is t))))
  .. (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>)
  ..   MY-TEST
  ..     ⋅ (IS T)
  ..   ⋅ MY-TEST ⋅1
  .. ⋅ (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) ⋅1
  ..
  ==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) EXPECTED-SUCCESS 0.000s ⋅1>
  ```

  In the example above, the [TESTABLE][@testables] argument is not the
  name of a test, so we see that TRY wraps an extra [TRIAL][class]
  around the lambda to ensure that the tree of TRIALs has a single
  root. This TRIAL object also remembers the lambda function for
  [rerunning][@rerun]. This situation also arises when there are
  multiple basic @TESTABLES.

  Explicit and implicit TRYs are very similar. The differences are
  that explicit TRY

  - can run @TESTABLES (it takes care of wrapping them in a function),
  - has a function argument for each of the *DEBUG*, *COLLECT*, etc
    variables.

  Those arguments default to *TRY-DEBUG*, *TRY-COLLECT*, etc, which
  parallel and default to *DEBUG*, *COLLECT*, etc if set to
  :UNSPECIFIED. *TRY-DEBUG* is NIL, the rest of them are :UNSPECIFIED.
  These defaults encourage the use of an explicit TRY call in the
  non-interactive case and calling the test functions directly in the
  interactive one, but this is not enforced in any way."
  (try function)
  (set-try-debug function)
  (*try-debug* variable)
  (*try-count* variable)
  (*try-collect* variable)
  (*try-rerun* variable)
  (*try-print* variable)
  (*try-describe* variable)
  (*try-stream* variable)
  (*try-printer* variable)
  (*n-recent-trials* variable)
  (recent-trial function)
  (! (variable nil))
  (!! (variable nil))
  (!!! (variable nil))
  (@testables section))

(define-try-var *try-debug* nil
  "The default value for TRY's :DEBUG argument.
  If :UNSPECIFIED, then the value of *DEBUG* is used instead.")
(define-try-var *try-count* :unspecified
  "The default value for TRY's :COUNT argument.
  If :UNSPECIFIED, then the value of *COUNT* is used instead.")
(define-try-var *try-collect* :unspecified
  "The default value for TRY's :COLLECT argument.
  If :UNSPECIFIED, then the value of *COLLECT* is used instead.")
(define-try-var *try-rerun* :unspecified
  "The default value for TRY's :RERUN argument.
  If :UNSPECIFIED, then the value of *RERUN* is used instead.")
(define-try-var *try-print* :unspecified
  "The default value for TRY's :PRINT argument.
  If :UNSPECIFIED, then the value of *PRINT* is used instead.")
(define-try-var *try-describe* :unspecified
  "The default value for TRY's :DESCRIBE argument.
  If :UNSPECIFIED, then the value of *DESCRIBE* is used instead.")
(define-try-var *try-stream* :unspecified
  "The default value for TRY's :STREAM argument.
  If :UNSPECIFIED, then the value of *STREAM* is used instead.")
(define-try-var *try-printer* :unspecified
  "The default value for TRY's :PRINTER argument.
  If :UNSPECIFIED, then the value of *PRINTER* is used instead.")

(defmacro try-default-unspecified-args (&key replayp)
  `(flet ((default-var (explicit-try-value implicit-try-value)
            (if (eq explicit-try-value :unspecified)
                implicit-try-value
                explicit-try-value)))
     (setq collect (default-var collect *collect*))
     ,@(unless replayp
         '((setq debug (default-var debug *debug*))
           (setq count (default-var count *count*))
           (setq rerun (default-var rerun *rerun*))))
     (setq print (default-var print *print*))
     (setq describe (default-var describe *describe*))
     (setq stream (default-var stream *stream*))
     (setq printer (default-var printer *printer*))))

(defmacro with-try-context (&body body)
  `(let* ((*internal-error* nil)
          ;; To detect TRY calls nested in this, from which no events
          ;; shall be handled.
          (try-id (gensym))
          (*try-id* try-id)
          ;; Break the ancestral chain across nested TRYs.
          (*trial* nil))
     ,@body))

;;; Check that user specified types are valid. Even after these
;;; checks, we use SAFE-TYPEP and SAFE-SUBTYPEP as they may become
;;; invalid while running, and the resulting cascade of errors is
;;; really unpleasant.
(defun check-event-type (type arg-name)
  (assert (valid-type-specifier-p type) ()
          "~@<~A, the ~S argument of ~S, must be a valid type specifier.~:@>"
          type arg-name 'try)
  ;; ECL thinks (SUBTYPEP '(AND EXPECTED SUCCESS) NIL) is true.
  ;; https://gitlab.com/embeddable-common-lisp/ecl/-/issues/664
  #-ecl
  (assert (or (eq type nil)
              (not (safe-subtypep `(and ,type event) nil)))
          () "~@<~A, the ~S argument of ~S, must be NIL ~
              or a type that intersects with ~S.~:@>"
          type arg-name 'try 'event))

(defun check-printer-arg (printer)
  (check-type printer symbol)
  (unless (subtypep printer 'printer)
    (error "~@<~S, the ~S argument of ~S, must be a subclass of ~S.~:@>"
           printer :printer 'try 'printer)))


(defvar *invoking-debugger* nil)

(defmacro with-current-debug ((var init-form) &body body)
  (with-gensyms (try-id stream value)
    `(let ((,var ,init-form)
           (,try-id *try-id*))
       (restart-bind
           ((:stream ,stream)
            (set-try-debug
              (,value)
              :report (format ,stream "Supply a new value for ~S of ~S."
                              :debug 'try)
              :test (and (eq *try-id* ,try-id) *invoking-debugger*)
              :interactive (read-try-debug)
              (setq ,var ,value)))
         ,@body))))

(defun read-try-debug ()
  (with-retry/go ()
    (handler-case
        (progn
          (format *query-io*
                  "~&~@<Enter a type for the ~S of the currently running ~S ~
                   (not evaluated):~:@> "
                  'debug 'try)
          (finish-output *query-io*)
          (let ((type (read *query-io*)))
            (check-event-type type :debug)
            (list type)))
      (error (c)
        (format *query-io* "~&~@<~S retrying on error ~A~:@>"
                'set-try-debug c)
        (go retry)))))

(defun set-try-debug (debug)
  "Invoke the SET-TRY-DEBUG restart to override the DEBUG argument of
  the currently running TRY. DEBUG must thus be a suitable type. When
  the SET-TRY-DEBUG restart is invoked interactively, DEBUG is read as
  a non-evaluated form from *QUERY-IO*."
  (dbg "(~S ~S)" 'set-try-debug debug)
  (invoke-restart 'set-try-debug debug))


(defvar *recent-trials* ())

(defun recent-trial (&optional (n 0))
  "Returns the `N`th most recent trial or NIL if there are not enough
  trials recorded. Every TRIAL returned by TRY gets pushed
  onto a list of trials, but only *N-RECENT-TRIALS* are kept."
  (elt *recent-trials* n))

(defvar *n-recent-trials* 3
  "See *RECENT-TRIALS*.")

(defvar ! nil
  "The most recent trial. Equivalent to `(RECENT-TRIAL 0)`.")
(defvar !! nil
  "Equivalent to `(RECENT-TRIAL 1)`.")
(defvar !!! nil
  "Equivalent to `(RECENT-TRIAL 2)`.")

(defmacro remembering-most-recent-trials (&body body)
  `(on-values (progn ,@body)
     (add-to-recent-trials *)))

(defun add-to-recent-trials (trial)
  (push trial *recent-trials*)
  (setq ! (first *recent-trials*))
  (setq !! (second *recent-trials*))
  (setq !!! (third *recent-trials*))
  (setq *recent-trials* (subseq* *recent-trials* 0 *n-recent-trials*))
  trial)


(defvar *allow-nested-try* nil)

(defun try (testable &key (debug *try-debug*) (count *try-count*)
            (collect *try-collect*) (rerun *try-rerun*)
            (print *try-print*) (describe *try-describe*)
            (stream *try-stream*) (printer *try-printer*))
  "TRY runs [TESTABLE][@testables] and handles the EVENTs to
  [count][@count], [collect][@collect], debug, [print][@print] the
  results of checks and trials, and to decide what tests to SKIP and
  what to [rerun][@rerun].

  DEBUG, COUNT, COLLECT, RERUN, PRINT, and DESCRIBE must all be valid
  specifiers for types that are either NIL (the empty type) or have a
  non-empty intersection with the type EVENT (e.g. T, OUTCOME,
  UNEXPECTED, VERDICT).

  TRY sets up a HANDLER-BIND handler for EVENTs and runs TESTABLE (see
  @TESTABLES). When an EVENT is signalled, the handler matches its
  type to the value of the DEBUG argument (in the sense of `(TYPEP
  EVENT DEBUG)`). If it matches, then the debugger is invoked with the
  event. In the debugger, the user has a number of restarts available
  to change (see @EVENT-RESTARTS, @OUTCOME-RESTARTS,
  @CHECK-RESTARTS, @TRIAL-RESTARTS, and SET-TRY-DEBUG.

  If the debugger is not invoked, TRY invokes the very first restart
  available, which is always RECORD-EVENT.

  Recording the event is performed as follows.

  - Outcome counts are updated (see @COUNT).
  - The event is passed to the collector (see @COLLECT).
  - The event is passed to the printer (see @PRINT).
  - Finally, when rerunning a trial (i.e. when TESTABLE is a trial),
    on a TRIAL-START event, the trial may be skipped (see @RERUN).

  TRY returns the values returned by the outermost trial. This is just
  the [TRIAL][class] object in the absence of an explicit RETURN or
  RETURN-FROM (see examples in @TESTS).

  If TRY is called within the dynamic extent of another TRY run, then
  it simply calls TESTABLE, ignores the other arguments and leaves
  event handling to the enclosing TRY."
  (try-default-unspecified-args)
  (loop for (type arg-name) in
        `((,debug :debug) (,count :count) (,collect :collect) (,rerun :rerun)
          (,print :print) (,describe :describe))
        do (check-event-type type arg-name))
  (check-printer-arg printer)
  (when (and *try-id* (not *allow-nested-try*))
    (return-from try (call-testable testable)))
  (multiple-value-setq (testable rerun)
    (munge-try-args-for-rerun-context testable rerun))
  (with-try-context
    (let ((collector (make-%collector :count-type count
                                      :collect-type collect))
          (printer (make-instance printer :stream stream
                                  :print-type print
                                  :describe-type describe))
          (rerun-trial-event-handler
            (when (and (not (eq rerun t))
                       (typep testable 'trial)
                       (not (runningp testable)))
              (%make-rerun-trial-event-handler testable rerun))))
      (with-current-debug (debug debug)
        (labels ((really-record-event (event)
                   (with-internal-errors
                     (dbg "Recording ~S." event)
                     (%count-and-collect-event collector event)
                     (%print-event printer event))))
          (let ((*record-event* #'really-record-event))
            (handler-bind
                ((event (lambda (event)
                          (when (eq *try-id* try-id)
                            (%maybe-invoke-debugger event debug)
                            (with-internal-errors
                              (when (and rerun-trial-event-handler
                                         (typep event 'trial-event))
                                ;; This may call SKIP-TRIAL.
                                (funcall rerun-trial-event-handler event))
                              (record-event))))))
              (%unwind-protect
                  (remembering-most-recent-trials
                    (call-testable testable))
                (finish-printing printer)))))))))

(defun %maybe-invoke-debugger (outcome debug-outcome-type)
  (when (safe-typep outcome debug-outcome-type)
    (let ((*invoking-debugger* t))
      (invoke-debugger outcome))))


(defun try-for-emacs (testable-string &key rerun implicit new-context)
  (uiop:symbol-call
   '#:swank '#:call-with-buffer-syntax
   nil
   (lambda ()
     (assert (member rerun '(nil :unspecified t)))
     (let ((testable (try-for-emacs/find-testable testable-string rerun)))
       (list (try-for-emacs-1 testable rerun implicit new-context)
             (let ((*print-readably* nil))
               (prin1-to-string testable)))))))

;;; Like !, but for Emacs.
(defvar *emacs-!* nil)

;;; Like *RERUN-CONTEXT*, but for Emacs.
(defvar *emacs-rerun-context* nil)

(deftype deftest-trial-start () '(satisfies deftest-trial-start-p))

(defun deftest-trial-start-p (event)
  (and (typep event 'trial-start)
       (deftest-trial-p (trial event))))

(defun try-for-emacs-1 (testable rerun implicit new-context)
  (with-output-to-string (out)
    ;; Documented in the Elisp function `mgl-try'.
    (let ((*rerun-context* (if new-context
                               *rerun-context*
                               *emacs-rerun-context*))
          ;; Ensure that rerunning in context can find everything.
          (*collect* 'trial-event)
          (*rerun* (if (eq rerun t) t *rerun*))
          (*stream* (make-broadcast-stream out *standard-output*))
          (*error-output* (make-broadcast-stream out *error-output*))
          ;; Override @PRINT settings so that Elisp can parse the
          ;; output. *PRINT-BACKTRACE* is the only safe one.
          (*printer* 'tree-printer)
          (*print-parent* t)
          (*print-indentation* :outline)
          (*print-duration* (if *print-duration* :after-marker nil))
          (*print-compactly* nil)
          (*defer-describe* nil)
          (*categories*
            (list*
             ;; For `mgl-try-mode-deftest-name-on-current-line'
             '(deftest-trial-start :marker "→")
             ;; For `mgl-try-next-unexpected' to skip over these
             '(unexpected-verdict-failure :marker "→⊠")
             (fancy-std-categories)))
          (! nil)
          (!! nil)
          (!!! nil)
          (*recent-trials* ())
          (*n-recent-trials* 1))
      (catch 'nlx-barrier
        (unwind-protect
             (let ((trial
                     (if implicit
                         ;; We need a single tree of events, but
                         ;; TESTABLE can be e.g. a list of function
                         ;; designators. Using TRY solves this, but
                         ;; then we must explicitly make it use the
                         ;; @IMPLICIT-TRY settings.
                         (try testable
                              :debug *debug*
                              :count *count*
                              :collect *collect*
                              :rerun *rerun*
                              :print *print*
                              :describe *describe*
                              :stream *stream*
                              :printer *printer*)
                         (try testable
                              :collect *collect*
                              :rerun (if (eq rerun t) t *try-rerun*)
                              :stream *stream*
                              :printer *printer*))))
               (setq *emacs-!* !)
               (when (or new-context (and (eq rerun t)
                                          (trialp testable)))
                 (setq *emacs-rerun-context* trial)))
          (throw 'nlx-barrier nil))))))

(defun try-for-emacs/find-testable (string rerun)
  (let ((form (read-from-string string)))
    ;; Interpret a single symbol as a function name for `mgl-try'.
    (if (and (not rerun) (symbolp form))
        (if (fboundp form)
            form
            (error "~@<~S is not ~S~:@>" form 'fboundp))
        (eval form))))
