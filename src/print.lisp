(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @print (:title "Printing Events")
  "TRY instantiates a printer of the type given by its PRINTER
  argument. All EVENTs recorded by TRY are sent to this printer. The
  printer then prints events that match the type given by the PRINT
  argument of TRY. Events that also match the DESCRIBE argument of TRY
  are printed with context information (see IS) and backtraces (see
  UNHANDLED-ERROR).

  Although the printing is primarily customized with global special
  variables, changing the value of those variables after the printer
  object is instantiated by TRY has no effect. This is to ensure
  consistent output with nested TRY calls of differing printer
  setups."
  (tree-printer class)
  (*print-parent* variable)
  (*print-indentation* variable)
  (*print-duration* variable)
  (*print-compactly* variable)
  (*defer-describe* variable))

;;; FIXME: document this properly.
#+nil
(defsection @print/extension-api (:title "Printer Extension API")
  (printer class)
  (stream-of (reader printer))
  (stream-line generic-function)
  (stream-column generic-function)
  (print-type (reader printer))
  (describe-type (reader printer))
  (trial-print-states (reader printer))
  (print-state class)
  (print-state-trial (structure-accessor print-state))
  (print-state-n-retries (structure-accessor print-state))
  (print-state-first-line-and-column (structure-accessor print-state))
  (print-event generic-function)
  (describe-leaf generic-function)
  (finish-printing generic-function))

(defun write-trial-counts (trial stream)
  (let* ((counter (counter trial))
         (counts (counter-counts counter))
         (categories (categories trial)))
    (when (some #'plusp counts)
      (loop for category in categories
            for i upfrom 0
            do (destructuring-bind (category-type &key marker) category
                 (declare (ignore category-type))
                 (let ((count (aref counts i)))
                   (when (plusp count)
                     (format stream " ~A~A" marker count))))))))


(defvar *print-parent* t
  """When an EVENT is signalled and its parent TRIAL's type matches
  *PRINT-PARENT*, the trial is printed as if its TRIAL-START matched
  the PRINT argument of TRY.

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print* 'leaf)
        (*print-parent* t))
    (with-test (t0)
      (is t)
      (is t)))
  .. T0
  ..   ⋅ (IS T)
  ..   ⋅ (IS T)
  .. ⋅ T0 ⋅2
  ..
  ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
  ```

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print* 'leaf)
        (*print-parent* nil))
    (with-test (t0)
      (is t)
      (is t)))
  .. ⋅ (IS T)
  .. ⋅ (IS T)
  ..
  ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
  ```

  *PRINT-PARENT* NIL combined with printing VERDICTs results in a flat
   output:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print* '(or leaf verdict))
        (*print-parent* nil))
    (with-test (outer)
      (with-test (inner)
        (is t :msg "inner-t"))
      (is t :msg "outer-t")))
  .. ⋅ inner-t
  .. ⋅ INNER ⋅1
  .. ⋅ outer-t
  .. ⋅ OUTER ⋅2
  ..
  ==> #<TRIAL (WITH-TEST (OUTER)) EXPECTED-SUCCESS 0.000s ⋅2>
  ```
  """)

(defclass printer ()
  ((stream
    :initform *debug-io* :initarg :stream :reader stream-of
    :documentation "The stream to which output shall be written.")
   (print-type
    :initform t :initarg :print-type :reader print-type
    :documentation "The PRINT argument of TRY.")
   (describe-type
    :initform t :initarg :describe-type :reader describe-type
    :documentation "The DESCRIBE argument of TRY.")
   (print-parent
    :initform *print-parent* :reader print-parent
    :documentation "The value of *PRINT-PARENT* when the printer was
    instantiated.")
   (variable-bindings :initform (append *event-print-bindings*
                                        `((*package* ,*package*)
                                          (*categories* ,*categories*)
                                          (*event-print-bindings* ()))))
   (trial-print-states
    :initform () :reader trial-print-states
    :documentation "A list "))
  (:documentation "PRINTER is the abstract base class for all printers."))

(defmethod initialize-instance :after ((printer printer)
                                       &key &allow-other-keys)
  ;; (%ensure-new-line (stream-of printer)) Gray streams don't really
  ;; work on ABCL. http://abcl.org/trac/ticket/373
  #-abcl
  (unless (typep (stream-of printer) 'trivial-gray-streams:fundamental-stream)
    (setf (slot-value printer 'stream)
          (make-instance 'line/column-character-output-stream
                         :stream (stream-of printer)))))

(defstruct print-state
  trial
  ;; Its N-RETRIES at the time when TRIAL-START was printed most
  ;; recently for TRIAL.
  n-retries
  ;; STREAM-LINE and STREAM-COLUMN right after TRIAL's TRIAL-START
  ;; was printed.
  first-line-and-column)

(defun print-state-trial-printed-p (print-state)
  (<= (n-retries (print-state-trial print-state))
      (print-state-n-retries print-state)))

(defun trial-printed-p (trial printer)
  (let ((print-state (find trial (trial-print-states printer)
                           :key #'print-state-trial)))
    (and print-state (print-state-trial-printed-p print-state))))

;;; The print depth of an EVENT is almost the number of trials (more
;;; precisely PRINT-STATEs) on TRIAL-PRINT-STATES that have been
;;; printed (PRINT-STATE-TRIAL-PRINTED-P).
;;;
;;; But to have a trial's TRIAL-START (including the retries) and the
;;; VERDICT events at the same level, then decrement the depth if we
;;; have already printed the trial.
(defun event-print-depth (event printer)
  (- (loop for print-state in (trial-print-states printer)
           count (print-state-trial-printed-p print-state))
     (let ((trial (if (typep event '(or trial-start verdict))
                      (trial event)
                      nil)))
       (if (and trial (trial-printed-p trial printer))
           1
           0))))

(defun %print-event (printer event)
  (with-slots (print-type stream variable-bindings trial-print-states) printer
    (with-bindings variable-bindings
      (etypecase event
        (trial-start
         (let ((print-states (trial-print-states printer))
               (trial (trial event)))
           ;; If the first element PRINT-STATES is not of TRIAL, then
           ;; add a new PRINT-STATE.
           (unless (and print-states
                        (eq (print-state-trial (first print-states))
                            trial))
             (if (endp print-states)
                 (assert (null (parent trial)))
                 (assert (eq (parent trial)
                             (print-state-trial (first print-states)))))
             (push (make-print-state :trial (trial event)
                                     :n-retries -1)
                   trial-print-states)))
         (setf (slot-value (trial event) 'trial-start) event)
         (when (safe-typep event print-type)
           (%ensure-trial-printed (trial event) printer)))
        (verdict
         (%print-verdict printer event)
         (pop trial-print-states))
        (leaf
         (%print-leaf printer event))))))

(defun %ensure-trial-printed (trial printer)
  (let ((stream (stream-of printer)))
    (declare (ignorable stream))
    (labels
        ((%ensure (ancestor trial-print-states)
           (when ancestor
             (let ((print-state (first trial-print-states)))
               (assert print-state)
               (assert (eq (print-state-trial print-state) ancestor))
               (cond ((trial-printed-p ancestor printer)
                      (return-from %ensure))
                     (t
                      (%ensure (parent ancestor) (rest trial-print-states))
                      (print-event printer (trial-start ancestor))
                      #-abcl
                      (setf (print-state-first-line-and-column print-state)
                            (list (stream-line stream)
                                  (stream-column stream)))
                      (setf (print-state-n-retries print-state)
                            (n-retries ancestor))))))))
      (%ensure trial (member trial (trial-print-states printer)
                             :key #'print-state-trial)))))

(defun ancestors-from-oldest (trial)
  (reverse (loop for trial1 = trial then (parent trial1)
                 while trial1
                 collect trial1)))

(defun %print-leaf (printer leaf)
  (with-slots (print-type describe-type) printer
    (let ((printp (safe-typep leaf print-type)))
      (when printp
        (when (safe-typep *trial* (print-parent printer))
          (%ensure-trial-printed *trial* printer))
        (if (safe-typep leaf describe-type)
            (describe-leaf printer leaf)
            (print-event printer leaf))))))

(defun %print-verdict (printer verdict)
  (with-slots (print-type describe-type) printer
    (let ((trial (trial verdict))
          (printp (safe-typep verdict print-type)))
      (assert (eq *trial* trial))
      (when (or printp (trial-printed-p trial printer))
        (when (safe-typep (parent trial) (print-parent printer))
          (%ensure-trial-printed (parent trial) printer))
        (print-event printer verdict)))))

(defgeneric print-event (printer trial))
(defgeneric describe-leaf (printer leaf))
(defgeneric finish-printing (printer)
  (:method :around (printer)
    (with-slots (variable-bindings) printer
      (with-bindings variable-bindings
        (call-next-method)))))


(defvar *print-indentation* 2
  "The number of spaces each printed TRIAL increases the indentation
  of its children.")

(defvar *print-duration* nil
  """If true, the number of seconds spent during execution is printed.

  ```cl-transcript (:check-consistency nil)
  (let ((*print-duration* t)
        (*debug* nil)
        (*describe* nil))
    (with-test (timed)
      (is (progn (sleep 0.3) t))
      (is (progn (sleep 0.2) t))
      (error "xxx")))
  ..        TIMED
  ..  0.300   ⋅ (IS (PROGN (SLEEP 0.3) T))
  ..  0.200   ⋅ (IS (PROGN (SLEEP 0.2) T))
  ..          ⊟ ""xxx (SIMPLE-ERROR)
  ..  0.504 ⊟ TIMED ⊟1 ⋅2
  ..
  ==> #<TRIAL (WITH-TEST (TIMED)) ABORT* 0.504s ⊟1 ⋅2>
  ```

  Timing is available for all OUTCOMEs (i.e. for @CHECKS and TRIALs).
  Checks generally measure the time spent during evaluation the form
  they are wrapping. Trials measure the time between TRIAL-START and
  the VERDICT.

  Timing information is not available for TRIAL-START and ERROR*
  events.
  """)

(defvar *print-compactly* nil
  """EVENTs whose type matches *PRINT-COMPACTLY* are printed less
  verbosely. LEAF events are printed only with their marker, and
  VERDICTs of trials without printed child trials are printed with `=>
  <MARKER>` (see *CATEGORIES*).

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print-compactly* t)
        (*debug* nil)
        (*describe* nil))
    (with-test (outer)
      (loop repeat 10 do (is t))
      (with-test (inner)
        (is t)
        (is nil)
        (error "xxx"))
      (loop repeat 10 do (is t))))
  .. OUTER ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
  ..   INNER ⋅⊠⊟ => ⊟
  ..   ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
  .. ⊠ OUTER ⊟1 ⊠1 ⋅21
  ..
  ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊟1 ⊠1 ⋅21>
  ```

  *PRINT-COMPACTLY* has no effect on events being `DESCRIBE`d.
  """)

(defvar *defer-describe* nil
  "When an EVENT is to be `*DESCRIBE*`d and its type matches
  *DEFER-DESCRIBE*, then instead of printing the often longish context
  information in the tree of events, it is deferred until after TRY
  has finished. The following example only prints LEAF events (due to
  *PRINT* and *PRINT-PARENT*) and in compact form (see
  *PRINT-COMPACTLY*), deferring description of events matching
  *DESCRIBE* until the end.

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print* 'leaf)
        (*print-parent* nil)
        (*print-compactly* t)
        (*defer-describe* t)
        (*debug* nil))
    (with-test (outer)
      (loop repeat 10 do (is t))
      (with-test (inner)
        (is (= (1+ 5) 7)))))
  .. ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⊠
  ..
  .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in OUTER INNER:
  .. (IS (= #1=(1+ 5) 7))
  .. where
  ..   #1# = 6
  ..
  ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅10>
  ```")

(defclass tree-printer (printer)
  ((indentation :initform *print-indentation* :reader indentation)
   (print-duration-p :initform *print-duration* :reader print-duration-p)
   (print-compactly :initform *print-compactly* :reader print-compactly)
   (defer-describe :initform *defer-describe* :reader defer-describe)
   (to-be-described :initform () :accessor to-be-described))
  (:documentation """TREE-PRINTER prints events in an indented
  tree-like structure, with each internal node corresponding to a
  TRIAL. This is the default printer (according to *PRINTER* and
  *TRY-PRINTER*) and currently the only one.

  The following example prints all @CONCRETE-EVENTS.

  ```cl-transcript (:dynenv try-transcript)
  (let ((*debug* nil)
        (*print* '(not trial-start))
        (*describe* nil))
    (with-test (verdict-abort*)
      (with-test (expected-verdict-success))
      (with-expected-outcome ('failure)
        (with-test (unexpected-verdict-success)))
      (handler-bind (((and verdict success) #'force-expected-failure))
        (with-test (expected-verdict-failure)))
      (handler-bind (((and verdict success) #'force-unexpected-failure))
        (with-test (unexpected-verdict-failure)))
      (with-test (verdict-skip)
        (skip-trial))
      (is t :msg "EXPECTED-RESULT-SUCCESS")
      (with-failure-expected ('failure)
        (is t :msg "UNEXPECTED-RESULT-SUCCESS")
        (is nil :msg "EXPECTED-RESULT-FAILURE"))
      (is nil :msg "UNEXPECTED-RESULT-FAILURE")
      (with-skip ()
        (is nil :msg "RESULT-SKIP"))
      (handler-bind (((and result success) #'abort-check))
        (is t :msg "RESULT-ABORT*"))
      (catch 'foo
        (with-test (nlx-test)
          (throw 'foo nil)))
      (error "UNHANDLED-ERROR")))
  .. VERDICT-ABORT*                       ; TRIAL-START
  ..   ⋅ EXPECTED-VERDICT-SUCCESS
  ..   ⊡ UNEXPECTED-VERDICT-SUCCESS
  ..   × EXPECTED-VERDICT-FAILURE
  ..   ⊠ UNEXPECTED-VERDICT-FAILURE
  ..   - VERDICT-SKIP
  ..   ⋅ EXPECTED-RESULT-SUCCESS
  ..   ⊡ UNEXPECTED-RESULT-SUCCESS
  ..   × EXPECTED-RESULT-FAILURE
  ..   ⊠ UNEXPECTED-RESULT-FAILURE
  ..   - RESULT-SKIP
  ..   ⊟ RESULT-ABORT*
  ..   NLX-TEST                           ; TRIAL-START
  ..     ⊟ non-local exit                 ; NLX
  ..   ⊟ NLX-TEST ⊟1                      ; VERDICT-ABORT*
  ..   ⊟ "UNHANDLED-ERROR" (SIMPLE-ERROR)
  .. ⊟ VERDICT-ABORT* ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1
  ..
  ==> #<TRIAL (WITH-TEST (VERDICT-ABORT*)) ABORT* 0.004s ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1>
  ```

  The `⊟3 ⊠1 ⊡1 -1 ×1 ⋅1` part is the counts for *CATEGORIES* printed
  with their markers.
  """))

(defun %print-indent-for (printer event)
  (write-string (%make-indent (indentation printer)
                              (event-print-depth event printer))
                (stream-of printer)))

(defun %make-indent (indentation depth)
  (cond ((numberp indentation)
         (make-string (* indentation depth) :initial-element #\Space))
        ((eq indentation :outline)
         (concatenate 'string
                      (make-string (+ depth 1) :initial-element #\*)
                      " "))
        (t
         (assert nil))))

;;; The depth of the printer is the number of trials (more precisely
;;; PRINT-STATEs) on TRIAL-PRINT-STATES that have been printed.
(defun printer-depth (printer)
  (loop for print-state in (trial-print-states printer)
        when (print-state-trial-printed-p print-state)
          do (return (1+ (depth (print-state-trial print-state))))
        finally (return 0)))

(defun print-event-header (printer event)
  (let ((stream (stream-of printer)))
    (%ensure-new-line stream)
    (when (print-duration-p printer)
      (if (typep event 'outcome)
          (print-duration (elapsed-seconds event) stream)
          (format stream "      "))
      (format stream " "))
    (%print-indent-for printer event)
    (let ((marker (category-marker event *categories*)))
      (when marker
        (format stream "~A " marker)))))

;;; Just in case the running tests wrote to STREAM as well.
(defun %ensure-new-line (stream)
  (format stream "~&"))

(defun print-duration (seconds stream)
  (format stream "~6,3F" seconds))

(defmethod print-event ((printer tree-printer) (event trial-start))
  (let ((trial (trial event))
        (stream (stream-of printer)))
    (print-event-header printer event)
    (if (stringp (test-name trial))
        (format stream "~A" (test-name trial))
        (format stream "~S" (test-name trial)))
    (when (plusp (n-retries trial))
      (format stream " retry #~S" (n-retries trial)))))

(defmethod print-event ((printer tree-printer) (verdict verdict))
  (let ((stream (stream-of printer))
        (print-state (first (trial-print-states printer)))
        (trial (trial verdict)))
    (assert (eq trial *trial*))
    (assert print-state)
    (assert (eq trial (print-state-trial print-state)))
    (let ((one-line-p (and (safe-typep verdict (print-compactly printer))
                           (not (print-duration-p printer))
                           (trial-printed-p trial printer)
                           #-abcl
                           (= (first (print-state-first-line-and-column
                                      print-state))
                              (stream-line stream)))))
      (cond (one-line-p
             (format stream " => ~A" (category-marker verdict *categories*)))
            (t
             (print-event-header printer verdict)
             (write-event verdict stream :terse t :ctx t))))
    (terpri stream)))

(defmethod print-event ((printer tree-printer) (leaf leaf))
  (let ((stream (stream-of printer))
        (print-state (first (trial-print-states printer))))
    (declare (ignorable print-state))
    (cond ((safe-typep leaf (print-compactly printer))
           #+abcl
           (progn
             (terpri stream)
             (%print-indent-for printer leaf))
           #-abcl
           (let ((column (stream-column (stream-of printer))))
             (cond ((<= (or *print-right-margin* 80) column)
                    (terpri stream)
                    (%print-indent-for printer leaf))
                   ((zerop column)
                    (%print-indent-for printer leaf))
                   ((equal (print-state-first-line-and-column print-state)
                           (list (stream-line stream)
                                 (stream-column stream)))
                    (format stream " ")))
             (format stream "~A" (category-marker leaf *categories*))))
          (t
           (print-event-header printer leaf)
           (with-circularity-detection (stream)
             (write-event leaf stream :terse t :ctx nil))
           (terpri stream)))))

(defmethod describe-leaf ((printer tree-printer) outcome)
  (cond ((safe-typep outcome (defer-describe printer))
         (print-event printer outcome)
         (push (list *trial* outcome) (to-be-described printer)))
        (t
         (with-slots (stream) printer
           (print-event-header printer outcome)
           (let ((*print-backtrace* t))
             (with-circularity-detection (stream)
               (write-event outcome stream :terse t :ctx t))
             (terpri stream))))))

(defmethod finish-printing ((printer tree-printer))
  (describe-to-be-described printer))

(defun describe-to-be-described (printer)
  (with-slots (stream) printer
    (loop for i upfrom 0
          for (trial outcome) in (reverse (to-be-described printer))
          do (format stream "~&~%")
             (pprint-logical-block (stream nil :per-line-prefix ";; ")
               (format stream "~@<~A (~A) in ~{~S~^ ~}:~:@>"
                       (type-of outcome)
                       (category-marker outcome *categories*)
                       (ancestor-trial-names trial)))
             (let ((*categories* nil))
               (setf (slot-value printer 'defer-describe) nil)
               (describe-leaf printer outcome)))))

(defun ancestor-trial-names (trial)
  (reverse (loop for trial1 = trial then (parent trial1)
                 while trial1
                 collect (test-name trial1))))
