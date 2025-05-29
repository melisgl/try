(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @events (:title "Events")
  "Try is built around events implemented as CONDITIONs.
  Matching the types of events to *DEBUG*, *COUNT*, *COLLECT*, *RERUN*,
  *PRINT*, and *DESCRIBE* is what gives Try its flexibility."
  (@middle-layer-of-events section)
  (@concrete-events section)
  (@event-glue section)
  (@printing-events section)
  (@event-restarts section)
  (@outcomes section)
  (@errors section)
  (@categories section))

(defsection @middle-layer-of-events (:title "Middle Layer of Events")
  """The event hierarchy is fairly involved, so let's start with the middle
  layer because it is smallest. The condition EVENT has 4 disjoint
  subclasses:

  - TRIAL-START, starting a TRIAL (by executing a [test][@TESTS]),

  - VERDICT, the OUTCOME of a TRIAL,

  - RESULT, the OUTCOME of a [check][@CHECKS], and

  - ERROR*, an unexpected CL:ERROR or unadorned [non-local exit][clhs].

  ```cl-transcript (:dynenv try-transcript)
  (let (;; We don't want to debug nor print a backtrace for the error below.
        (*debug* nil)
        (*describe* nil))
    ;; signals TRIAL-START / VERDICT-ABORT* on entry / exit
    (with-test (demo)
      ;; signals EXPECTED-RESULT-SUCCESS
      (is t)
      ;; signals UNHANDLED-ERROR with a nested CL:ERROR
      (error "xxx")))
  .. DEMO                       ; TRIAL-START
  ..   ⋅ (IS T)                 ; EXPECTED-RESULT-SUCCESS (⋅)
  ..   ⊟ "xxx" (SIMPLE-ERROR)   ; UNHANDLED-ERROR (⊟)
  .. ⊟ DEMO ⊟1 ⋅1               ; VERDICT-ABORT* (⊟)
  ..
  ==> #<TRIAL (WITH-TEST (DEMO)) ABORT* 0.004s ⊟1 ⋅1>
  ```
  """)

(defsection @concrete-events (:title "Concrete Events")
  """The non-abstract condition classes of events that are actually
  signalled are called concrete.

  @CHECKS' RESULTs and @TRIALS' VERDICTs have six concrete subclasses
  each:

  - EXPECTED-RESULT-SUCCESS, UNEXPECTED-RESULT-SUCCESS,
    EXPECTED-RESULT-FAILURE, UNEXPECTED-RESULT-FAILURE,
    RESULT-SKIP, RESULT-ABORT*

  - EXPECTED-VERDICT-SUCCESS, UNEXPECTED-VERDICT-SUCCESS,
    EXPECTED-VERDICT-FAILURE, UNEXPECTED-VERDICT-FAILURE,
    VERDICT-SKIP, VERDICT-ABORT*

  Breaking the symmetry between @CHECKS and @TRIALS, TRIAL-START is a
  concrete event class, that marks the start of a TRIAL.

  ERROR* is an abstract class with two concrete subclasses:

  - UNHANDLED-ERROR, signalled when a CL:ERROR reaches the handler set
    up by DEFTEST or WITH-TEST, or when the debugger is invoked.

  - NLX, signalled when no error was detected by the handler, but the
    trial finishes with a [non-local exit][clhs].

 These are the 15 concrete event classes.
 """
  (concrete-events-of-type function))

(defvar *concrete-events*
  '(trial-start
    expected-result-success unexpected-result-success
    expected-result-failure unexpected-result-failure
    result-skip result-abort*
    expected-verdict-success unexpected-verdict-success
    expected-verdict-failure unexpected-verdict-failure
    verdict-skip verdict-abort*
    unhandled-error nlx))

(defun concrete-events-of-type (type)
  "The hierarchy of @EVENTS is hairy. Sometimes it's handy to list the
  @CONCRETE-EVENTS that match a given type. We use this below in the
  documentation."
  (loop for event-type in *concrete-events*
        ;; SUBTYPEP cannot deduce all true relationships, so make a
        ;; condition and use TYPEP.
        when (typep (make-condition event-type) type)
          collect event-type))

(defsection @event-glue (:title "Event Glue")
  "These condition classes group various bits of the @CONCRETE-EVENTS
   and the @MIDDLE-LAYER-OF-EVENTS for ease of reference.

  Concrete event classes except TRIAL-START and NLX are subclasses of
  the hyphen-separated words constituting their name. For example,
  UNEXPECTED-RESULT-FAILURE inherits from UNEXPECTED, RESULT, and
  FAILURE, so it matches types such as UNEXPECTED or `(AND UNEXPECTED
  RESULT)`."
  (event condition)
  (act condition)
  "EXPECTED and UNEXPECTED partition ACT."
  (expected condition)
  (unexpected condition)
  "SUCCESS, FAILURE and DISMISSAL partition ACT."
  (success condition)
  (failure condition)
  (dismissal condition)
  "ABORT* and SKIP partition DISMISSAL."
  (abort* condition)
  (skip condition)
  (leaf condition)
  "The following types are shorthands."
  (expected-success type)
  (unexpected-success type)
  (expected-failure type)
  (unexpected-failure type)
  "PASS and FAIL partition ACT."
  (pass type)
  (fail type))

(defmacro define-combi-event (basic-superclasses)
  (let ((name (intern-hyphenated basic-superclasses)))
    `(define-condition ,name ,(mapcar #'intern-hyphenated
                               (combine-superclasses basic-superclasses))
       ())))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun combine-superclasses (basic-superclasses)
    (let ((n (length basic-superclasses)))
      (assert (< 1 n))
      (let ((combinations ()))
        (alexandria:map-combinations (lambda (combination)
                                       (push combination combinations))
                                     basic-superclasses :length 1)
        (reverse combinations)))))

(define-condition event () ()
  (:documentation "Common abstract superclass of all events in Try."))

(define-condition act (event) ()
  (:documentation "EVENTs that produce evidence or determine the
  course of a TRIAL are ACTs. All events are ACTs except TRIAL-START.

  ```cl-transcript
  (concrete-events-of-type '(not act))
  => (TRIAL-START)
  ```"))

(define-condition expected (act) ()
  (:documentation "Concrete condition classes with EXPECTED in their
  name are subclasses of EXPECTED. SKIP is also a subclass of
  EXPECTED.

  ```cl-transcript
  (concrete-events-of-type 'expected)
  => (EXPECTED-RESULT-SUCCESS EXPECTED-RESULT-FAILURE RESULT-SKIP
      EXPECTED-VERDICT-SUCCESS EXPECTED-VERDICT-FAILURE VERDICT-SKIP)
  ```"))

(define-condition unexpected (act) ()
  (:documentation "Concrete condition classes with UNEXPECTED in their
  name are subclasses of UNEXPECTED. ABORT* is also a subclass of
  UNEXPECTED.

  ```cl-transcript
  (concrete-events-of-type 'unexpected)
  => (UNEXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-FAILURE RESULT-ABORT*
      UNEXPECTED-VERDICT-SUCCESS UNEXPECTED-VERDICT-FAILURE
      VERDICT-ABORT* UNHANDLED-ERROR NLX)
  ```"))

(define-condition success (act) ()
  (:documentation "See @CHECKS and @TRIAL-VERDICTS for how
  SUCCESS or FAILURE is decided.

  ```cl-transcript
  (concrete-events-of-type 'success)
  => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
      EXPECTED-VERDICT-SUCCESS UNEXPECTED-VERDICT-SUCCESS)
  ```"))

(define-condition failure (act) ()
  (:documentation "See SUCCESS.

  ```cl-transcript
  (concrete-events-of-type 'failure)
  => (EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE
      EXPECTED-VERDICT-FAILURE UNEXPECTED-VERDICT-FAILURE)
  ```"))

(define-condition dismissal (act) ()
  (:documentation "The third possibility after SUCCESS and FAILURE.
  Either SKIP or ABORT*.

  ```cl-transcript
  (concrete-events-of-type 'dismissal)
  => (RESULT-SKIP RESULT-ABORT* VERDICT-SKIP VERDICT-ABORT*
      UNHANDLED-ERROR NLX)
  ```"))

(define-condition abort* (unexpected dismissal) ()
  (:documentation "```cl-transcript
  (concrete-events-of-type 'abort*)
  => (RESULT-ABORT* VERDICT-ABORT* UNHANDLED-ERROR NLX)
  ```"))

(define-condition leaf (act) ()
  (:documentation "Event that do not mark a TRIAL's
  start (TRIAL-START) or end (VERDICT) are LEAF events. These are the
  leafs of the tree of nested trials delineated by their TRIAL-START
  and VERDICT events.

  ```cl-transcript
  (concrete-events-of-type 'leaf)
  => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
      EXPECTED-RESULT-FAILURE UNEXPECTED-RESULT-FAILURE RESULT-SKIP
      RESULT-ABORT* UNHANDLED-ERROR NLX)
  ```

  LEAF EVENTs are RESULTs of @CHECKS and also ERROR*s.

  ```cl-transcript
  (equal (concrete-events-of-type 'leaf)
         (concrete-events-of-type '(or result error*)))
  => T
  ```

  Equivalently, LEAF is the complement of TRIAL-EVENT.

  ```cl-transcript
  (equal (concrete-events-of-type 'leaf)
         (concrete-events-of-type '(not trial-event)))
  => T
  ```"))


(deftype expected-success ()
  "A shorthand for `(AND EXPECTED SUCCESS)`."
  '(and expected success))
(deftype unexpected-success ()
  "A shorthand for `(AND UNEXPECTED SUCCESS)`."
  '(and unexpected success))
(deftype expected-failure ()
  "A shorthand for `(AND EXPECTED FAILURE)`."
  '(and expected failure))
(deftype unexpected-failure ()
  "A shorthand for `(AND UNEXPECTED FAILURE)`."
  '(and unexpected failure))

(deftype pass ()
  "An OUTCOME that's not an ABORT* or an UNEXPECTED-FAILURE.
  PASS is equivalent to `(NOT FAIL)`. PASSes are signalled with
  SIGNAL.

  ```cl-transcript
  (concrete-events-of-type 'pass)
  => (EXPECTED-RESULT-SUCCESS UNEXPECTED-RESULT-SUCCESS
      EXPECTED-RESULT-FAILURE RESULT-SKIP EXPECTED-VERDICT-SUCCESS
      UNEXPECTED-VERDICT-SUCCESS EXPECTED-VERDICT-FAILURE VERDICT-SKIP)
  ```"
  '(and outcome (not (or abort* unexpected-failure))))

(deftype fail ()
  "An ABORT* or an UNEXPECTED-FAILURE. FAIL conditions are signalled
  with [ERROR][function]. See PASS.

  ```cl-transcript
  (concrete-events-of-type 'fail)
  => (UNEXPECTED-RESULT-FAILURE RESULT-ABORT* UNEXPECTED-VERDICT-FAILURE
      VERDICT-ABORT* UNHANDLED-ERROR NLX)
  ```"
  '(or abort* unexpected-failure))


(defsection @printing-events (:title "Printing Events")
  (*event-print-bindings* variable))

(define-try-var *event-print-bindings* '((*print-circle* t))
  "EVENTs are conditions signalled in code that may change printer
  variables such as *PRINT-CIRCLE*, *PRINT-LENGTH*, etc. To control
  how events are printed, the list of variable bindings in
  *EVENT-PRINT-BINDINGS* is established whenever an EVENT is printed
  as if with:

  ```
  (progv (mapcar #'first *event-print-bindings*)
         (mapcar #'second *event-print-bindings*)
    ...)
  ```

  The default value ensures that shared structure is recognized (see
  @CAPTURES). If the `#N#` syntax feels cumbersome, then change this
  variable.")

(defmethod print-object ((event event) stream)
  (if *print-escape*
      (print-unreadable-object (event stream :type t)
        (ignore-errors (write-event event stream :terse t :ctx nil)))
      ;; Reporting the condition in the debugger comes here.
      (ignore-errors
       (write-event event stream :terse nil :ctx t))))

(defgeneric write-event (event stream &key terse ctx)
  (:method :around (event stream &key terse ctx)
    ;; FIXME: *PRINT-CIRCLE* seems to have no effect if bound here.
    (with-bindings *event-print-bindings*
      ;; We rely on *PRINT-PRETTY*, force it on regardless of
      ;; *EVENT-PRINT-BINDINGS*. KLUDGE: pretty printing on CLISP is
      ;; very broken.
      (let ((*print-pretty* #-clisp t #+clisp nil))
        (call-next-method event stream :terse terse :ctx ctx)))))


(defsection @event-restarts (:title "Event Restarts")
  "Only RECORD-EVENT is applicable to all EVENTs. See
  @CHECK-RESTARTS, @TRIAL-RESTARTS for more."
  (record-event function))

(defun record-event (&optional condition)
  "This restart is always the first restart available when an EVENT is
  signalled running under TRY (i.e. there is a CURRENT-TRIAL). TRY
  always invokes RECORD-EVENT when handling events."
  (declare (ignore condition))
  (invoke-restart 'record-event))

(defun signal-with-record-event (condition)
  (let ((try-id *try-id*))
    (restart-case
        (signal condition)
      ()
      (record-event ()
        :report "Record event and continue."
        :test (eq *try-id* try-id)
        (dbg "Restart ~S called on ~S." 'record-event condition)
        (funcall *record-event* condition)))))


(defsection @categories (:title "Categories")
  """Categories determine how event types are printed and events of
  what types are counted together.

  The default value of *CATEGORIES* is

  ```
  ((abort*             :marker "⊟")
   (unexpected-failure :marker "⊠")
   (unexpected-success :marker "⊡")
   (skip               :marker "-")
   (expected-failure   :marker "×")
   (expected-success   :marker "⋅"))
  ```

  which says that all concrete EVENTs that are of type ABORT* (i.e.
  `RESULT-ABORT*`, `VERDICT-ABORT*`, UNHANDLED-ERROR, and NLX) are to
  be marked with `"⊟"` when printed (see @PRINT). Also, the six
  types define six counters for @COUNT. Note that UNEXPECTED events
  have the same marker as their EXPECTED counterpart but squared."""
  (*categories* (variable "- see above -"))
  (fancy-std-categories function)
  (ascii-std-categories function))

(defun fancy-std-categories ()
  "Returns the default value of *CATEGORIES* (see @CATEGORIES),
  which contains some fancy Unicode characters."
  '((abort*             :marker "⊟")
    (unexpected-failure :marker "⊠")
    (unexpected-success :marker "⊡")
    (skip               :marker "-")
    (expected-failure   :marker "×")
    (expected-success   :marker "⋅")))

(defun ascii-std-categories ()
  """Returns a value suitable for *CATEGORIES*, which uses only ASCII
  characters for the markers.

  ```
  '((abort*             :marker "!")
    (unexpected-failure :marker "F")
    (unexpected-success :marker ":")
    (skip               :marker "-")
    (expected-failure   :marker "f")
    (expected-success   :marker "."))
  ```
  """
  '((abort*             :marker "!")
    (unexpected-failure :marker "F")
    (unexpected-success :marker ":")
    (skip               :marker "-")
    (expected-failure   :marker "f")
    (expected-success   :marker ".")))

;;; Ordered by some notion of importance.
(define-try-var *categories* (fancy-std-categories)
  "A list of of elements like `(TYPE &KEY MARKER)`. When @PRINT,
  @CONCRETE-EVENTS are printed with the marker of the first matching
  type. When @COUNT, the counts associated with all matching types are
  incremented.")

(defun event-category (event categories)
  (if (typep event 'event)
      (loop for category in categories
            for i upfrom 0
            do (destructuring-bind (type &key marker) category
                 (when (typep event type)
                   (return (values type marker i)))))
      (loop for category in categories
            for i upfrom 0
            do (destructuring-bind (type &key marker) category
                 (when (safe-subtypep event type)
                   (return (values type marker i)))))))

(defun category-marker (event categories)
  (nth-value 1 (event-category event categories)))
