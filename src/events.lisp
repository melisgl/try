(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try/events (:title "Events")
  "Try is built around events implemented as CONDITIONs.
  Matching the types of events to *DEBUG*, *COUNT*, *COLLECT*, *RERUN*,
  *PRINT*, and *DESCRIBE* is what gives Try its flexibility."
  (@try/middle-layer-of-events section)
  (@try/concrete-events section)
  (@try/event-glue section)
  (@try/printing-events section)
  (@try/event-restarts section)
  (@try/outcomes section)
  (@try/errors section)
  (@try/categories section))

(defsection @try/middle-layer-of-events (:title "Middle Layer of Events")
  """The event hierarchy is fairly involved, so let's start in the middle.
  The condition EVENT has 4 disjoint subclasses:

  - TRIAL-START, which corresponds to the entry to a test (see
    @TRY/TESTS),

  - VERDICT, the OUTCOME of a TRIAL,

  - RESULT, the OUTCOME of a check (see @TRY/CHECKS), and

  - ERROR*, an unexpected CL:ERROR or unadorned @NON-LOCAL-EXIT.

  ```
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

(defsection @try/concrete-events (:title "Concrete Events")
 """The non-abstract condition classes of events that are actually
 signalled are called concrete.

  TRIAL-START is a concrete event class. RESULTs and VERDICTs have six
  concrete subclasses:

  - EXPECTED-RESULT-SUCCESS, UNEXPECTED-RESULT-SUCCESS,
    EXPECTED-RESULT-FAILURE, UNEXPECTED-RESULT-FAILURE,
    RESULT-SKIP, RESULT-ABORT*

  - EXPECTED-VERDICT-SUCCESS, UNEXPECTED-VERDICT-SUCCESS,
    EXPECTED-VERDICT-FAILURE, UNEXPECTED-VERDICT-FAILURE,
    VERDICT-SKIP, VERDICT-ABORT*

  ERROR* is an abstract class with two concrete subclasses:

  - UNHANDLED-ERROR, signalled when a CL:ERROR reaches the handler set
    up by DEFTEST or WITH-TEST, or when the debugger is invoked.

  - NLX, signalled when no error was detected by the handler, but the
    trial finishes with a @NON-LOCAL-EXIT.

 These are the 15 concrete event classes.
 """)

(defsection @try/event-glue (:title "Event Glue")
  "These condition classes group various bits of the
  @TRY/CONCRETE-EVENTS and the @TRY/MIDDLE-LAYER-OF-EVENTS for ease of
  reference.

  Concrete event classes except TRIAL-START are subclasses of
  hyphen-separated words in their name. For example,
  UNEXPECTED-RESULT-FAILURE inherits from UNEXPECTED, RESULT, and
  FAILURE, so it matches types such as UNEXPECTED or `(AND UNEXPECTED
  RESULT)`."
  (event condition)
  (expected condition)
  (unexpected condition)
  (success condition)
  (failure condition)
  (dismissal condition)
  (abort* condition)
  (skip condition)
  (leaf condition)
  (expected-success type)
  (unexpected-success type)
  (expected-failure type)
  (unexpected-failure type)
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

(define-condition leaf (event) ()
  (:documentation "RESULT or ERROR*."))

(define-condition expected (event) ()
  (:documentation "Concrete condition classes with EXPECTED in their
  name are subclasses of EXPECTED. SKIP is also a subclass of
  EXPECTED."))

(define-condition unexpected (event) ()
  (:documentation "Concrete condition classes with UNEXPECTED in their
  name are subclasses of UNEXPECTED. ABORT* is also a subclass of
  UNEXPECTED."))

(define-condition success (event) ()
  (:documentation "See @TRY/CHECKS and @TRY/TRIAL-VERDICTS for how
  SUCCESS or FAILURE is decided."))

(define-condition failure (event) ()
  (:documentation "See SUCCESS."))

(define-condition dismissal (event) ()
  (:documentation "The third possibility after SUCCESS and FAILURE.
  Either SKIP or ABORT*."))

(define-condition abort* (unexpected) ()
  (:documentation "`RESULT-ABORT*`, `VERDICT-ABORT*` or ERROR*."))


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
  "An OUTCOME that's not an ABORT* or an UNEXPECTED FAILURE."
  '(and outcome (not (or abort* unexpected-failure))))
(deftype fail ()
  "An ABORT* or an UNEXPECTED FAILURE."
  '(or abort* unexpected-failure))


(defsection @try/printing-events (:title "Printing Events")
  (*event-print-bindings* variable))

(defvar *event-print-bindings* '((*print-circle* t))
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
  @TRY/CAPTURES). If the `#N#` syntax feels cumbersome, then change
  this variable.")

(defmethod print-object ((event event) stream)
  (if *print-escape*
      (print-unreadable-object (event stream :type t)
        (ignore-errors (write-event event stream :terse t :ctx nil)))
      ;; Reporting the condition in the debugger comes here.
      (ignore-errors
       (write-event event stream :terse nil :ctx t))))

(defgeneric write-event (event stream &key terse ctx)
  (:method :around (event stream &key terse ctx)
    (with-bindings *event-print-bindings*
      ;; We rely *PRINT-PRETTY*, force it on regardless of
      ;; *EVENT-PRINT-BINDINGS*. KLUDGE: pretty printing on CLISP is
      ;; very broken.
      (let ((*print-pretty* #-clisp t #+clisp nil))
        (call-next-method event stream :terse terse :ctx ctx)))))


(defsection @try/event-restarts (:title "Event Restarts")
  "Only RECORD-EVENT is applicable to all EVENTs. See
  @TRY/CHECK-RESTARTS, @TRY/TRIAL-RESTARTS for more."
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


(defsection @try/categories (:title "Categories")
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
  be marked with `"⊟"` when printed (see @TRY/PRINT). Also, the six
  types define six counters for @TRY/COUNT. Note that UNEXPECTED
  events have the same marker but squared as their EXPECTED
  counterpart.
  """
  (*categories* (variable "- see above -"))
  (fancy-std-categories function)
  (ascii-std-categories function))

(defun fancy-std-categories ()
  "Returns the default value of *CATEGORIES* (see @TRY/CATEGORIES),
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
(defvar *categories* (fancy-std-categories)
  "A list of of elements like `(TYPE &KEY MARKER)`.
  When @TRY/PRINT, @TRY/CONCRETE-EVENTS are printed with the marker of
  the first matching type. When @TRY/COUNT, the counts associated with
  all matching types are incremented.")

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
