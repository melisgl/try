(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @check-library (:title "Check Library")
  "In the following, various checks built on top of IS are described.
  Many of them share a number of arguments, which are described here.

  - ON-RETURN is a boolean that determines whether the check in a
    macro that wraps BODY is made when BODY returns normally.

  - ON-NLX is a boolean that determines whether the check in a macro
    that wraps BODY is made when BODY performs a [non-local exit][clhs].

  - MSG and CTX are @FORMAT-SPECIFIER-FORMS as in IS.

  - NAME may be provided so that it is printed (with PRIN1) instead of
    BODY in MSG."
  (@checking-conditions section)
  (@misc-checks section)
  (@check-utilities section))


(defsection @checking-conditions (:title "Checking Conditions")
  """The macros SIGNALS, SIGNALS-NOT, INVOKES-DEBUGGER, and
  INVOKES-DEBUGGER-NOT all check whether a condition of a given type,
  possibly also matching a predicate, was signalled. In addition to
  those already described in @CHECK-LIBRARY, these macros share a
  number of arguments.

  Matching conditions are those that are of type CONDITION-TYPE (not
  evaluated) and satisfy the predicate PRED.

  When PRED is NIL, it always matches. When it is a string, then it
  matches if it is a substring of the printed representation of the
  condition being handled (by PRINC under WITH-STANDARD-IO-SYNTAX).
  When it is a function, it matches if it returns true when called
  with the condition as its argument.

  The check is performed in the cleanup form of an UNWIND-PROTECT
  around BODY.

  HANDLER is called when a matching condition is found. It can be a
  function, T, or NIL. When it is a function, it is called from the
  condition handler (SIGNALS and SIGNALS-NOT) or the debugger
  hook (INVOKES-DEBUGGER and INVOKES-DEBUGGER-NOT) with the matching
  condition. HANDLER may perform a [non-local exit][clhs]. When
  HANDLER is T, the matching condition is handled by performing a
  non-local exit to just outside BODY. If the exit completes, BODY is
  treated as if it had returned normally, and ON-RETURN is consulted.
  When HANDLER is NIL, no addition action is performed when a matching
  condition is found.

  The default CTX describes the result of the matching process in
  terms of *CONDITION-MATCHED-P* and *BEST-MATCHING-CONDITION*.
  """
  (*condition-matched-p* variable)
  (*best-matching-condition* variable)
  (signals macro)
  (signals-not macro)
  (invokes-debugger macro)
  (invokes-debugger-not macro))

(defmacro with-condition-matching ((matchedp &optional best-match)
                                   (condition-type &key pred handler)
                                   &body body)
  (multiple-value-bind (body declarations) (alexandria:parse-body body)
    (let ((best-match (or best-match (make-gensym '#:best-match))))
      (with-gensyms (best-match-degree degree c)
        `(let ((,matchedp nil)
               (,best-match-degree 0)
               (,best-match nil))
           ,@declarations
           (flet ((match-condition (,c)
                    (let ((,degree (%match-condition ,c ',condition-type
                                                     ,pred)))
                      (when (< ,best-match-degree ,degree)
                        (setq ,best-match-degree ,degree)
                        (setq ,best-match ,c))
                      (when (<= 2 ,degree)
                        (setq ,matchedp t)
                        ,@(when handler
                            `((funcall ,handler ,c)))))))
             ,@body))))))


(defvar *condition-matched-p*)
(setf (documentation '*condition-matched-p* 'variable)
      "When a check described in @CHECKING-CONDITIONS signals its
      OUTCOME, this variable is bound to a boolean value to indicate
      whether a condition that matched CONDITION-TYPE and PRED was
      found.")

(defvar *best-matching-condition*)
(setf (documentation '*best-matching-condition* 'variable)
      "Bound when a check described in @CHECKING-CONDITIONS
      signals its OUTCOME. If *CONDITION-MATCHED-P*, then it is the
      most recent condition that matched both CONDITION-TYPE and PRED.
      Else, it is the most recent condition that matched
      CONDITION-TYPE or NIL if no such conditions were detected.")

(defmacro condition-match-checker ((condition-type &key pred (handler t)
                                    (on-return t) (on-nlx t)
                                    name msg ctx check-function)
                                   &body body)
  (with-gensyms (matchedp best-match handled %retry finishedp)
    `(with-retry/go (:retry ,%retry)
       (with-timing
         (with-condition-matching (,matchedp ,best-match)
             (,condition-type :pred ,pred :handler ,(if (eq handler t)
                                                        `(throw ',handled nil)
                                                        handler))
           (on-finish
               (catch ',handled
                 ,@body)
             (lambda (,finishedp)
               (when (or (and ,finishedp ,on-return)
                         (and (not ,finishedp) ,on-nlx))
                 (when (eq :retry
                           (let ((*condition-matched-p* ,matchedp)
                                 (*best-matching-condition* ,best-match))
                             (,check-function
                              ',condition-type ',pred ,name
                              ,(canonicalize-format-specifier-form msg)
                              ,(canonicalize-format-specifier-form ctx)
                              ',body)))
                   (go ,%retry))))))))))

(defun %match-condition (condition condition-type pred)
  (cond ((not (typep condition condition-type))
         0)
        (t (cond ((null pred) 2)
                 ((typep pred 'string)
                  (if (search pred (%describe-condition-for-matching
                                    condition))
                      2
                      1))
                 (t
                  (if (funcall pred condition) 2 1))))))

(defun %describe-condition-for-matching (condition &optional stream)
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (if (typep condition 'simple-condition)
          (apply #'format stream
                 (simple-condition-format-control condition)
                 (simple-condition-format-arguments condition))
          (format stream "~A" condition)))))


(defmacro signals ((condition-type &key pred (handler t)
                    (on-return t) (on-nlx t) name msg ctx)
                   &body body)
  """Check that BODY signals a CONDITION of CONDITION-TYPE (not
  evaluated) that matches PRED. To detect matching conditions, SIGNALS
  sets up a HANDLER-BIND. Thus it can only see what BODY does not
  handle. The arguments are described in @CHECKING-CONDITIONS.

  ```cl-transcript (:dynenv try-transcript)
  (signals (error)
    (error "xxx"))
  => NIL
  ```

  The following example shows a failure where CONDITION-TYPE matches
  but PRED does not.

  ```cl-transcript (:dynenv try-transcript)
  (signals (error :pred "non-matching")
    (error "xxx"))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (ERROR "xxx") signals a condition of type ERROR that matches
  ..     "non-matching".
  ..   The predicate did not match "xxx".
  ```
  """
  `(condition-match-checker (,condition-type
                             :pred ,pred :handler ,handler
                             :on-return ,on-return :on-nlx ,on-nlx
                             :name ,(or name `',(present-body body))
                             :msg ,msg :ctx ,ctx
                             :check-function check-signals)
     (handler-bind ((,condition-type #'match-condition))
       (progn ,@body))))

(defun check-signals (condition-type pred name msg ctx body)
  (is *condition-matched-p*
      :msg (or msg
               (list "~S signals a condition of type ~S~@[ that matches ~S~]."
                     (or name (present-body body)) condition-type pred))
      :ctx (or ctx
               (cond (*condition-matched-p*
                      nil)
                     (*best-matching-condition*
                      (list "The predicate did not match ~S."
                            (%describe-condition-for-matching
                             *best-matching-condition*)))
                     (t
                      (list "No condition of type ~S was received."
                            condition-type))))
      :capture nil
      :retry nil))


(defmacro signals-not ((condition-type &key pred (handler t)
                        (on-return t) (on-nlx t) name msg ctx)
                       &body body)
  "Check that BODY does not signal a CONDITION of CONDITION-TYPE (not
  evaluated) that matches PRED. To detect matching conditions,
  SIGNALS-NOT sets up a HANDLER-BIND. Thus, it can only see what BODY
  does not handle. The arguments are described in
  @CHECKING-CONDITIONS."
  `(condition-match-checker (,condition-type
                             :pred ,pred :handler ,handler
                             :on-return ,on-return :on-nlx ,on-nlx
                             :name ,(or name `',(present-body body))
                             :msg ,msg :ctx ,ctx
                             :check-function check-signals-not)
     (handler-bind ((,condition-type #'match-condition))
       (progn ,@body))))

(defun check-signals-not (condition-type pred name msg ctx body)
  (is (null *condition-matched-p*)
      :msg (or msg
               (list "~S does not signal a condition of type ~S~
                      ~@[ that match ~S~]."
                     (or name (present-body body)) condition-type pred))
      :ctx (or ctx
               (list "Condition received: \"~A\" (~S)"
                     *best-matching-condition*
                     (type-of *best-matching-condition*)))
      :capture nil
      :retry nil))


(defmacro invokes-debugger ((condition-type &key pred (handler t)
                             (on-return t) (on-nlx t) name msg ctx)
                            &body body)
  """Check that BODY enters the debugger with a CONDITION of
  CONDITION-TYPE (not evaluated) that matches PRED. To detect matching
  conditions, INVOKES-DEBUGGER sets up a *DEBUGGER-HOOK*. Thus, if
  *DEBUGGER-HOOK* is changed by BODY, it may not detect the condition.
  The arguments are described in @CHECKING-CONDITIONS.

  Note that in a trial (see CURRENT-TRIAL), all ERRORs are handled,
  and a *DEBUGGER-HOOK* is set up (see UNHANDLED-ERROR). Thus,
  invoking debugger would normally cause the trial to abort.

  ```cl-transcript (:dynenv try-transcript)
  (invokes-debugger (error :pred "xxx")
    (handler-bind ((error #'invoke-debugger))
      (error "xxx")))
  => NIL
  ```
  """
  `(condition-match-checker (,condition-type
                             :pred ,pred :handler ,handler
                             :on-return ,on-return :on-nlx ,on-nlx
                             :name ,(or name `',(present-body body))
                             :msg ,msg :ctx ,ctx
                             :check-function check-invokes-debugger)
     (with-debugger-hook #'match-condition
       ,@body)))

(defun check-invokes-debugger (condition-type pred name msg ctx body)
  (is *condition-matched-p*
      :msg (or msg (invokes-debugger-default-msg name body condition-type
                                                 pred))
      :ctx (or ctx (invokes-debugger-default-ctx condition-type))
      :capture nil
      :retry nil))

(defun invokes-debugger-default-msg (name body condition-type pred)
  (list "~S invokes the debugger with a condition of type ~S~
         ~@[ that matches ~S~]."
        (or name (present-body body)) condition-type pred))

(defun invokes-debugger-default-ctx (condition-type)
  (cond (*condition-matched-p*
         nil)
        (*best-matching-condition*
         (list "The predicate did not match ~S."
               (%describe-condition-for-matching *best-matching-condition*)))
        (t
         (list "No condition of type ~S reached the debugger."
               condition-type))))


(defmacro invokes-debugger-not ((condition-type &key pred (handler t)
                                 (on-return t) (on-nlx t) name msg ctx)
                                &body body)
  "Check that BODY does not enter the debugger with a CONDITION of
  CONDITION-TYPE (not evaluated) that matches PRED. To detect matching
  conditions, INVOKES-DEBUGGER-NOT sets up a *DEBUGGER-HOOK*. Thus, if
  *DEBUGGER-HOOK* is changed by BODY, it may not detect the condition.
  The arguments are described in @CHECKING-CONDITIONS."
  `(condition-match-checker (,condition-type
                             :pred ,pred :handler ,handler
                             :on-return ,on-return :on-nlx ,on-nlx
                             :name ,(or name `',(present-body body))
                             :msg ,msg :ctx ,ctx
                             :check-function check-invokes-debugger-not)
     (with-debugger-hook #'match-condition
       ,@body)))

(defun check-invokes-debugger-not (condition-type pred name msg ctx body)
  (is (not *condition-matched-p*)
      :msg (or msg (invokes-debugger-not-default-msg name body condition-type
                                                     pred))
      :ctx (or ctx (invokes-debugger-default-ctx condition-type))
      :capture nil
      :retry nil))

(defun invokes-debugger-not-default-msg (name body condition-type pred)
  (list "~S does not invoke the debugger with a condition of type ~S~
         ~@[ that matches ~S~]."
        (or name (present-body body)) condition-type pred))


(defsection @misc-checks (:title "Miscellaneous Checks")
  (fails macro)
  (in-time macro)
  (*in-time-elapsed-seconds* variable))


(defmacro fails ((&key name msg ctx) &body body)
  """Check that BODY performs a [non-local exit][clhs] but do not
  cancel it (see @CANCELLED-NLX). See @CHECK-LIBRARY for the
  descriptions of the other arguments.

  In the following example, FAILS signals a SUCCESS.

  ```cl-transcript (:dynenv try-transcript)
  (catch 'foo
    (fails ()
      (throw 'foo 7)))
  => 7
  ```

  Next, FAILS signals an UNEXPECTED-FAILURE because BODY returns
  normally.

  ```cl-transcript (:dynenv try-transcript)
  (fails ()
    (print 'hey))
  ..
  .. HEY 
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (PRINT 'HEY) does not return normally.
  ```

  Note that there is no `FAILS-NOT` as WITH-TEST fills that role.
  """
  (with-gensyms (%retry finishedp)
    `(with-retry/go (:retry ,%retry)
       (with-timing
         (on-finish
             (progn ,@body)
           (lambda (,finishedp)
             (when (eq :retry (check-fails
                               ,finishedp ,name
                               ,(canonicalize-format-specifier-form msg)
                               ,(canonicalize-format-specifier-form ctx)
                               ',body))
               (go ,%retry))))))))

(defun check-fails (finishedp name msg ctx body)
  (is (not finishedp)
      :msg (or msg (list "~S does not return normally."
                         (or name (present-body body))))
      :ctx ctx
      :retry nil))


(defmacro in-time ((seconds &key (on-return t) (on-nlx t) name msg ctx)
                   &body body)
  """Check that BODY finishes in SECONDS. See @CHECK-LIBRARY for
  the descriptions of the other arguments.

  ```
  (in-time (1)
    (sleep 2))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (SLEEP 2) finishes within 1s.
  ..   Took 2.000s.
  ```

  RETRY-CHECK restarts timing.
  """
  (with-gensyms (%retry finishedp)
    `(with-retry/go (:retry ,%retry)
       (with-timing
         (on-finish (progn ,@body)
           (lambda (,finishedp)
             (when (or (and ,finishedp ,on-return)
                       (and (not ,finishedp) ,on-nlx))
               (when (eq :retry
                         (let ((*in-time-elapsed-seconds*
                                 (get-elapsed-seconds)))
                           (is (<= *in-time-elapsed-seconds* ,seconds)
                               :msg (or ,msg
                                        (list "~A finishes within ~Ss."
                                              (or ',name (present-body ',body))
                                              ,seconds))
                               :ctx (or ,ctx (list "Took ~,3Fs."
                                                   *in-time-elapsed-seconds*))
                               :capture nil)))
                 (go ,%retry)))))))))

(defvar *in-time-elapsed-seconds*)
(setf (documentation '*in-time-elapsed-seconds* 'variable)
      "Bound to the number of seconds passed during the evaluation of
      BODY when IN-TIME signals its OUTCOME.")


(defsection @check-utilities (:title "Check Utilities")
  "These utilities are not checks (which signal OUTCOMEs) but simple
  functions and macros that may be useful for writing IS checks."
  (on-values macro)
  (match-values macro)
  (mismatch% function)
  (different-elements function)
  (same-set-p function)
  (with-shuffling macro)
  (@comparing-floats section))


(defmacro match-values (form &body body)
  """MATCH-VALUES returns true iff all return values of FORM satisfy
  the predicates given by BODY, which are described in ON-VALUES. The
  :ON-LENGTH-MISMATCH and :TRUNCATE options of ON-VALUES are
  supported. If no :ON-LENGTH-MISMATCH option is specified, then
  MATCH-VALUES returns NIL on length mismatch.

  ```cl-transcript (:dynenv try-transcript)
  ;; no values
  (is (match-values (values)))
  ;; single value success
  (is (match-values 1
        (= * 1)))
  ;; success with different types
  (is (match-values (values 1 "sdf")
        (= * 1)
        (string= * "sdf")))
  ;; too few values
  (is (not (match-values 1
             (= * 1)
             (string= * "sdf"))))
  ;; too many values
  (is (not (match-values (values 1 "sdf" 3)
             (= * 1)
             (string= * "sdf"))))
  ;; too many values, but truncated
  (is (match-values (values 1 "sdf" 3)
        (:truncate t)
        (= * 1)
        (string= * "sdf")))
  ```
  """
  `(every #'identity (multiple-value-list
                      (on-values ,form
                        ,@(unless (%extract-options '(:on-length-mismatch) body
                                                    :options-first t)
                            '((:on-length-mismatch (return nil))))
                        ,@body))))

(defmacro match-values% (form &body body)
  `(every #'identity (multiple-value-list
                      (on-values (capture-values ,form)
                        ,@(unless (%extract-options '(:on-length-mismatch) body
                                                    :options-first t)
                            '((:on-length-mismatch (return nil))))
                        ,@body))))

;;; Get VALUES-FORM from (MATCH-VALUES VALUES-FORM ...).
(defmethod substitute-is-list-form ((first (eql 'match-values)) form env)
  (declare (ignore env))
  (values `(match-values% ,@(rest form)) nil))


;;; The TEST-NOT argument is not included because CCL complains about
;;; conflicting options TEST and TEST-NOT even if TEST-NOT is NIL and
;;; it's deprecated according to the CLHS.
(defun mismatch% (sequence1 sequence2 &key from-end (test #'eql)
                  (start1 0) end1 (start2 0) end2 key
                  max-prefix-length max-suffix-length)
  """Like CL:MISMATCH but CAPTUREs and returns the common prefix and
  the mismatched suffixes. The `TEST-NOT` argument is deprecated by
  the CLHS and is not supported. In addition, if MAX-PREFIX-LENGTH and
  MAX-SUFFIX-LENGTH are non-`NIL`, they must be non-negative integers,
  and they limit the number of elements in the prefix and the
  suffixes.

  ```cl-transcript (:dynenv try-transcript)
  (is (null (mismatch% '(1 2 3) '(1 2 4 5))))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (NULL #1=(MISMATCH% '(1 2 3) '(1 2 4 5))))
  ..   where
  ..     COMMON-PREFIX = (1 2)
  ..     MISMATCHED-SUFFIX-1 = (3)
  ..     MISMATCHED-SUFFIX-2 = (4 5)
  ..     #1# = 2
  ```

  ```cl-transcript (:dynenv try-transcript)
  (is (null (mismatch% "Hello, World!"
                       "Hello, world!")))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (NULL #1=(MISMATCH% "Hello, World!" "Hello, world!")))
  ..   where
  ..     COMMON-PREFIX = "Hello, "
  ..     MISMATCHED-SUFFIX-1 = "World!"
  ..     MISMATCHED-SUFFIX-2 = "world!"
  ..     #1# = 7
  ```
  """
  (let ((mismatch-position
          (mismatch sequence1 sequence2
                    :from-end from-end :test test
                    :start1 start1 :end1 end1 :start2 start2 :end2 end2
                    :key #+abcl
                    (or key #'identity)
                    #-abcl
                    key)))
    (if mismatch-position
        (let ((common-prefix
                (subseq* sequence1
                         (if max-prefix-length
                             (- mismatch-position max-prefix-length)
                             0)
                         mismatch-position))
              (mismatched-suffix-1
                (subseq* (subseq sequence1 mismatch-position)
                         0 max-suffix-length))
              (mismatched-suffix-2
                (subseq* (subseq sequence2 mismatch-position)
                         0 max-suffix-length)))
          (values mismatch-position
                  (capture common-prefix)
                  (capture mismatched-suffix-1)
                  (capture mismatched-suffix-2)))
        nil)))


(defun different-elements (sequence1 sequence2 &key (pred #'eql)
                           (missing :missing))
  "Return the different elements under PRED in the given sequences as
  a list of `(:INDEX <INDEX> <E1> <E2>)` elements, where `E1` and `E2`
  are elements of SEQUENCE1 and SEQUENCE2 at `<INDEX>`, respectively,
  and they may be MISSING if the corresponding sequence is too short.

  ```cl-transcript (:dynenv try-transcript)
  (is (endp (different-elements '(1 2 3) '(1 b 3 d))))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (ENDP #1=(DIFFERENT-ELEMENTS '(1 2 3) '(1 B 3 D))))
  ..   where
  ..     #1# = ((:INDEX 1 2 B) (:INDEX 3 :MISSING D))
  ```"
  (let ((n1 (length sequence1))
        (n2 (length sequence2)))
    (loop for i below (max n1 n2)
          for x1 = (if (< i n1)
                       (elt sequence1 i)
                       missing)
          for x2 = (if (< i n2)
                       (elt sequence2 i)
                       missing)
          unless (funcall pred x1 x2)
            collect (list :index i x1 x2))))


(defun same-set-p (list1 list2 &key key (test #'eql))
  """See if LIST1 and LIST2 represent the same set.
  See CL:SET-DIFFERENCE for a description of the KEY and TEST arguments.

  ```cl-transcript (:dynenv try-transcript)
  (try:is (try:same-set-p '(1) '(2)))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (SAME-SET-P '(1) '(2)))
  ..   where
  ..     ONLY-IN-1 = (1)
  ..     ONLY-IN-2 = (2)
  ```"""
  (let ((only-in-1 (set-difference list1 list2 :key key :test test))
        (only-in-2 (set-difference list2 list1 :key key :test test)))
    ;; Always capture both.
    (capture only-in-1)
    (capture only-in-2)
    (and (endp only-in-1) (endp only-in-2))))


;;; Lifted from Parachute.
(defmacro with-shuffling (() &body body)
  "Execute the forms that make up the list of forms BODY in random
  order and return NIL. This may be useful to prevent writing tests
  that accidentally depend on the order in which subtests are called.

  ```cl-transcript (:check-consistency nil)
  (loop repeat 3 do
    (with-shuffling ()
      (prin1 1)
      (prin1 2)))
  .. 122112
  => NIL
  ```"
  (with-gensyms (thunk)
    `(dolist (,thunk (shuffle
                      (list ,@(loop for form in body
                                    collect `(lambda () ,form)))))
       (funcall ,thunk))))
