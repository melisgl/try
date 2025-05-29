(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @is (:title "The IS Macro")
  """IS is the fundamental one among @CHECKS, on which all
  the others are built, and it is a replacement for CL:ASSERT that can
  capture values of subforms to provide context to failures:

  ```cl-transcript (:dynenv try-transcript)
  (is (= (1+ 5) 0))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (= #1=(1+ 5) 0))
  ..   where
  ..     #1# = 6
  ```

  IS automatically captures values of arguments to functions like `1+`
  in the above example. Values of other interesting subforms can be
  explicitly requested to be captured. IS supports capturing multiple
  values and can be taught how to deal with macros. The combination of
  these features allows MATCH-VALUES to be implementable as tiny
  extension:

  ```cl-transcript (:dynenv try-transcript)
  (is (match-values (values (1+ 5) "sdf")
        (= * 0)
        (string= * "sdf")))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS
  ..      (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
  ..        (= * 0)
  ..        (STRING= * "sdf")))
  ..   where
  ..     #1# == 6
  ..            #2#
  ```

  IS is flexible enough that all other checks (SIGNALS, SIGNALS-NOT,
  INVOKES-DEBUGGER, INVOKES-DEBUGGER-NOT, FAILS, and IN-TIME are built
  on top of it.
  """
  (is macro)
  (*is-form* variable)
  (*is-captures* variable)
  (@format-specifier-forms section)
  (@captures section))

(defvar *is-form*)
(setf (documentation '*is-form* 'variable)
      "IS binds this to its FORM argument for CTX and MSG.")
(defvar *is-captures*)
(setf (documentation '*is-captures* 'variable)
      "Captures made during an IS evaluation are made available for
      CTX via *IS-CAPTURES*.")

(defmacro is (form &key msg ctx (capture t) (print-captures t) (retry t)
              &environment env)
  """If FORM returns NIL, signal a RESULT FAILURE. Else, signal a
  RESULT SUCCESS. IS returns normally if

  - the RECORD-EVENT restart is invoked (available when in a trial), or

  - the CONTINUE restart is invoked (available when not in a trial), or

  - the condition signalled last (after @OUTCOME-RESTARTS) is a PASS,
    and it is not [handle][clhs]d.

  If IS returns normally after signalling an OUTCOME, it returns T if
  the last condition signalled was a SUCCESS, and NIL otherwise.

  - MSG and CTX are @FORMAT-SPECIFIER-FORMS. MSG prints a description
    of the check being made, which is by default the whole IS form.
    Due to how conditions are printed, MSG says what the desired
    outcome is, and CTX provides information about the evaluation.

      ```cl-transcript (:check-consistency #+sbcl t #-sbcl nil)
      (is (equal (prin1-to-string 'hello) "hello")
          :msg "Symbols are replacements for strings."
          :ctx ("*PACKAGE* is ~S and *PRINT-CASE* is ~S~%"
                *package* *print-case*))
      .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
      ..   UNEXPECTED-FAILURE in check:
      ..     Symbols are replacements for strings.
      ..   where
      ..     (PRIN1-TO-STRING 'HELLO) = "HELLO"
      ..   *PACKAGE* is #<PACKAGE "TRY"> and *PRINT-CASE* is :UPCASE
      ..
      ```

  - If CAPTURE is true, the value(s) of some subforms of FORM may be
    automatically recorded in the condition and also made available
    for CTX via *IS-CAPTURES*. See @CAPTURES for more.

  - If PRINT-CAPTURES is true, the captures made are printed when the
    RESULT condition is displayed in the debugger or
    `*DESCRIBE*`d (see @PRINT). This is the `where (PRIN1-TO-STRING
    'HELLO) ="HELLO"` part above. If PRINT-CAPTURES is NIL, the
    captures are still available in *IS-CAPTURES* for writing custom
    CTX messages.

  - If RETRY is true, then the RETRY-CHECK restart evaluates FORM
    again and signals a new RESULT. If RETRY is NIL, then the
    RETRY-CHECK restart returns :RETRY, which allows complex checks
    such as SIGNALS to implement their own retry mechanism."""
  (with-gensyms (%form %succesp)
    (multiple-value-bind (is-substituted-form subs)
        (if capture
            (substitute-is-form form env)
            (values form ()))
      (let ((signal-form
              `(signal-is-outcome
                ,%succesp
                ',form
                ,print-captures
                ,(canonicalize-format-specifier-form msg)
                ,(canonicalize-format-specifier-form ctx))))
        (let ((%retry-name (if retry (make-gensym '#:retry) nil)))
          `(macrolet ((% (,%form)
                        `(capture ,,%form))
                      (%% (,%form)
                        `(capture-values ,,%form)))
             (with-retry/go (:retry ,%retry-name)
               (with-timing
                 (let* ((*is-captures* ())
                        ;; The above binding of *IS-CAPTURES* may be
                        ;; modified by CAPTURE during the evaluation
                        ;; these bindings.
                        ,@(%subs-to-bindings subs)
                        (*is-form* ',form)
                        ,@(when subs
                            `((*is-captures* (nconc ,(%subs-to-captures subs)
                                                    *is-captures*))))
                        ;; This can change *IS-CAPTURES* via CAPTURE.
                        (,%succesp ,is-substituted-form))
                   ,(if retry
                        `(case ,signal-form
                           (:retry (go ,%retry-name))
                           ((t) t))
                        signal-form))))))))))

(defun signal-is-outcome (succesp form print-captures msg ctx)
  (signal-outcome t (cond (*skip* 'skip)
                          (succesp 'success)
                          (t 'failure))
                  (list
                   :check `(is ,form)
                   :elapsed-seconds (get-elapsed-seconds)
                   :captures (setq *is-captures*
                                   (scrub-captures (nreverse *is-captures*)))
                   :print-captures print-captures
                   :msg msg
                   :ctx ctx)))


(defsection @format-specifier-forms (:title "Format Specifier Forms")
  """A format specifier form is a Lisp form, typically an argument to
  macro, standing for the FORMAT-CONTROL and FORMAT-ARGS arguments to
  the FORMAT function.

  It may be a constant string:

  ```cl-transcript (:dynenv try-transcript)
  (is nil :msg "FORMAT-CONTROL~%with no args.")
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     FORMAT-CONTROL
  ..     with no args.
  ```

  It may be a list whose first element is a constant string, and the
  rest are the format arguments to be evaluated:

  ```cl-transcript (:dynenv try-transcript)
  (is nil :msg ("Implicit LIST ~A." "form"))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     Implicit LIST form.
  ```

  Or it may be a form that evaluates to a list like `(FORMAT-CONTROL
  &REST FORMAT-ARGS)`:

  ```cl-transcript (:dynenv try-transcript)
  (is nil :msg (list "Full ~A." "form"))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     Full form.
  ```

  Finally, it may evaluate to NIL, in which case some context specific
  default is implied.
  """
  (canonicalize-format-specifier-form function))

(defun canonicalize-format-specifier-form (form)
  "Ensure that the format specifier form FORM is in its full form."
  (cond ((stringp form)
         `(list ,form))
        ((and (listp form)
              (stringp (first form)))
         (cons 'list form))
        (t
         form)))


(defsection @captures (:title "Captures")
  "During the evaluation of the FORM argument of IS, evaluation of any
  form (e.g. a subform of FORM) may be recorded, which are called
  captures."
  (@automatic-captures section)
  (@explicit-captures section))


(defsection @explicit-captures (:title "Explicit Captures")
  """In addition to automatic captures, which are prescribed by
  rewriting rules (see @WRITING-AUTOMATIC-CAPTURE-RULES), explicit,
  ad-hoc captures can also be made.

  ```cl-transcript (:dynenv try-transcript)
  (is (let ((x 1))
        (= (capture x) 2)))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS
  ..      (LET ((X 1))
  ..        (= (CAPTURE X) 2)))
  ..   where
  ..     X = 1
  ```

  If CAPTURE showing up in the form that IS prints is undesirable,
  then `%` may be used instead:

  ```cl-transcript (:dynenv try-transcript)
  (is (let ((x 1))
        (= (% x) 2)))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS
  ..      (LET ((X 1))
  ..        (= X 2)))
  ..   where
  ..     X = 1
  ```

  Multiple values may be captured with CAPTURE-VALUES and its
  secretive counterpart `%%`:

  ```cl-transcript (:dynenv try-transcript)
  (is (= (%% (values 1 2)) 2))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (= #1=(VALUES 1 2) 2))
  ..   where
  ..     #1# == 1
  ..            2
  ```

  where printing `==` instead of [=][dislocated] indicates that this
  is a multiple value capture.
  """
  (capture macro)
  (capture-values macro)
  (% macrolet)
  (%% macrolet))

(defmacro capture (form)
  "Evaluate FORM, record its primary return value if within the
  dynamic extent of an IS evaluation, and finally return that value.
  If CAPTURE is used within the lexical scope of IS, then CAPTURE
  itself will show up in the form that the default MSG prints. Thus it
  is recommended to use the equivalent MACROLET `%` in the lexical
  scope as `%` is removed before printing."
  (with-gensyms (%value)
    `(let ((,%value ,form))
       (when (boundp '*is-captures*)
         (push (list ',form ,%value nil t) *is-captures*))
       ,%value)))

(defmacro capture-values (form)
  "Like CAPTURE-VALUES, but record and return all values returned by
  FORM. It is recommended to use the equivalent MACROLET `%%` in the
  lexical scope as `%%` is removed before printing."
  (with-gensyms (%values)
    `(let ((,%values (multiple-value-list ,form)))
       (when (boundp '*is-captures*)
         (push (list ',form ,%values t t) *is-captures*))
       (values-list ,%values))))

(dref-ext:define-symbol-locative-type macrolet ())

(dref-ext:define-definer-for-symbol-locative-type define-macrolet macrolet)

(define-macrolet % (form)
  "An alias for CAPTURE in the lexical scope of IS. Removed from the
  IS form when printed.")

(define-macrolet %% (form)
  "An alias for CAPTURE-VALUES in the lexical scope of IS. Removed
  from the IS form when printed.")



(defsection @automatic-captures (:title "Automatic Captures")
  """IS automatically captures some subforms of FORM that are likely
  to be informative. In particular, if FORM is a function call, then
  non-constant arguments are automatically captured:

  ```cl-transcript (:check-consistency #+sbcl t #-sbcl nil)
  (is (= 3 (1+ 2) (- 4 3)))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (= 3 #1=(1+ 2) #2=(- 4 3)))
  ..   where
  ..     #1# = 3
  ..     #2# = 1
  ```

  By default, automatic captures are not made for subforms deeper in
  FORM, except for when FORM is a call to [NULL][function],
  [ENDP][function] and [NOT][function]:

  ```cl-transcript (:check-consistency #+sbcl t #-sbcl nil)
  (is (null (find (1+ 1) '(1 2 3))))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (NULL #1=(FIND #2=(1+ 1) '(1 2 3))))
  ..   where
  ..     #2# = 2
  ..     #1# = 2
  ```

  ```cl-transcript (:check-consistency #+sbcl t #-sbcl nil)
  (is (endp (member (1+ 1) '(1 2 3))))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (ENDP #1=(MEMBER #2=(1+ 1) '(1 2 3))))
  ..   where
  ..     #2# = 2
  ..     #1# = (2 3)
  ```

  Note that the argument of [NOT][function] is not captured as it is
  assumed to be NIL or T. If that's not true, use [NULL][function].

  ```cl-transcript (:dynenv try-transcript)
  (is (not (equal (1+ 5) 6)))
  .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
  ..   UNEXPECTED-FAILURE in check:
  ..     (IS (NOT (EQUAL #1=(1+ 5) 6)))
  ..   where
  ..     #1# = 6
  ```

  Other automatic captures are discussed with the relevant
  functionality such as MATCH-VALUES.
  """
  (@writing-automatic-capture-rules section))

(defstruct (sub (:constructor make-sub (var subform new-form valuesp)))
  "A SUB (short for substitution) says that in the original form IS is
  checking, a SUBFORM was substituted (by SUBSTITUTE-IS-FORM) with
  VAR (if VALUESP is NIL) or with (VALUES-LIST VAR) if VALUESP is
  true. Conversely, VAR is to be bound to the evaluated NEW-FORM if
  VALUESP is NIL, and to (MULTIPLE-VALUE-LIST FORM) if VALUESP.
  NEW-FORM is often `EQ` to SUBFORM, but it may be different, which is
  the case when further substitutions are made within a substitution."
  var
  subform
  new-form
  valuesp)

;;; A CAPTURE records evaluation of a subform of the original form IS
;;; is checking. It says that SUBFORM evaluated to VALUE (if VALUESP
;;; is NIL) or that (MULTIPLE-VALUE-LIST SUBFORM) evaluated to VALUE
;;; (if VALUESP). If EXPLICITP then this capture was made by CAPTURE,
;;; CAPTURE-VALUES (or the equivalent %, %%), else the capture was
;;; created from a SUB made by SUBSTITUTE-IS-FORM.
(defstruct (capture
            (:type list)
            (:constructor make-capture (subform value valuesp explicitp)))
  subform
  value
  valuesp
  explicitp)

;;; Return an expression that evaluates to a list of
;;; (<substituted-expression> <evaluated-substituted-expression>)
;;; lists.
;;;
;;; (%subs-to-bindings '((g1 (1+ 3) nil) (g2 (values 1 2) t))) => ((G1
;;; (1+ 3)) (G2 (MULTIPLE-VALUE-LIST (VALUES 1 2))))
(defun %subs-to-bindings (subs)
  (loop for sub in subs
        collect `(,(sub-var sub)
                  ,(if (sub-valuesp sub)
                       `(multiple-value-list ,(sub-new-form sub))
                       (sub-new-form sub)))))

(defun %subs-to-captures (subs)
  `(list ,@(loop for sub in (reverse subs)
                 collect `(list ',(sub-subform sub)
                                ,(sub-var sub)
                                ,(sub-valuesp sub)
                                nil))))

;;; Deduplicate implicit CAPTUREs with the same SUBFORM and VALUESP.
;;; Then remove implicit captures for which there exists a capture
;;; with the same SUBFORM and the same VALUESP or VALUESP true.
;;;
;;; Note that explicit captures are never removed.
;;;
;;; SUBFORMs are compared with EQ since in printed CHECKs we rely on
;;; *PRINT-CIRCLE*.
(defun scrub-captures (captures)
  (let ((captures (remove-duplicates
                   (strip-outer-%-and-%%-from-implicit-captures captures)
                   :test #'same-but-different-implicit-captures-p)))
    (flet ((dominated-capture-p (capture)
             (destructuring-bind (subform var valuesp explicitp) capture
               (declare (ignore var))
               (and (not explicitp)
                    (find-if (lambda (other-capture)
                               (and (not (eq capture other-capture))
                                    (eq (capture-subform other-capture)
                                        subform)
                                    (or (capture-valuesp other-capture)
                                        (not valuesp))))
                             captures)))))
      (remove-if #'dominated-capture-p captures))))

(defun same-but-different-implicit-captures-p (capture1 capture2)
  (and (not (eq capture1 capture2))
       (not (capture-explicitp capture1))
       (not (capture-explicitp capture2))
       (eq (capture-subform capture1) (capture-subform capture2))
       (bool= (capture-valuesp capture1) (capture-valuesp capture2))))

;;; This allows SCRUB-CAPTURES to find identical captures made
;;; explicitly by % or %% and implicitly by SUBSTITUTE-IS-FORM.
(defun strip-outer-%-and-%%-from-implicit-captures (captures)
  (mapcar (lambda (capture)
            (destructuring-bind (subform var valuesp explicitp) capture
              (if (and (listp subform) (not explicitp)
                       (member (first subform) '(% capture
                                                 %% capture-values)))
                  (make-capture (second subform) var valuesp explicitp)
                  capture)))
          captures))


(defsection @writing-automatic-capture-rules
    (:title "Writing Automatic Capture Rules")
  (sub class)
  (make-sub function)
  (sub-var (structure-accessor sub))
  (sub-subform (structure-accessor sub))
  (sub-new-form (structure-accessor sub))
  (sub-valuesp (structure-accessor sub))
  (substitute-is-list-form generic-function))

(defun substitute-is-form (form env)
  (if (atom form)
      form
      (on-values (substitute-is-list-form (first form) form env)
        (or * form) *)))

(defgeneric substitute-is-list-form (first form env)
  (:documentation "In the list FORM, whose CAR is FIRST, substitute
  subexpressions of interest with a GENSYM and return the new form. As
  the second value, return a list of SUBs.

  For example, consider `(IS (FIND (FOO) LIST))`. When
  SUBSTITUTE-IS-LIST-FORM is invoked on `(FIND (FOO) LIST)`, it
  substitutes each argument of FIND with a variable, returning the new
  form `(FIND TEMP1 TEMP2)` and the list of two
  substitutions `((TEMP2 (FOO) (FOO) NIL) (TEMP3 LIST LIST NIL))`.
  This allows the original form to be rewritten as

  ```
  (let* ((temp1 (foo))
         (temp2 list))
    (find temp1 temp2))
  ```

  `TEMP1` and `TEMP2` may then be reported in the OUTCOME condition
  signalled by IS like this:

      The following check failed:
        (is (find #1=(foo) #2=list))
      where
        #1# = <return-value-of-foo>
        #2# = <value-of-variable-list>"))

;;; For normal functions like #'=, make a SUB for every argument form
;;; that's not CONSTANTISHP.
;;;
;;; E.g. (= (1+ 3) 4 (1- 5)) -> (= #:G1 4 #:G2) ((#:G1 (1+ 3) nil)
;;;                             (#:G2 (1- 5) nil))
(defmethod substitute-is-list-form (first form env)
  (when (and (not (constantishp form))
             (not (macro-function first env))
             (not (special-operator-p first))
             (not (member first '(% %%))))
    (substitute-args form)))

;;; Like CONSTANTP but returns NIL for lists that are not NIL, QUOTE
;;; or FUNCTION forms.
(defun constantishp (form)
  (cond ((null form)
         t)
        ((listp form)
         (and (= (length form) 2)
              (or (eq (first form) 'quote)
                  (eq (first form) 'function))))
        (t
         (constantp form))))

(defun substitute-args (form)
  (let ((arg-forms ())
        (subs ()))
    (loop for arg-form in (rest form)
          do (if (constantishp arg-form)
                 (push arg-form arg-forms)
                 (let ((temp-var (gensym #.(symbol-name 'temp))))
                   (push (make-sub temp-var arg-form arg-form nil) subs)
                   (push temp-var arg-forms))))
    (values `(,(first form) ,@(reverse arg-forms))
            ;; in evaluation order
            (reverse subs))))

;;; Now, consider (IS (NULL (FIND (FOO) LIST))), where we want to make
;;; nested substitutions that capture the return value of FIND as well
;;; as its arguments similar to the explicit capture (IS (NULL (%
;;; (FIND (% (FOO)) (% LIST))))). When SUBSTITUTE-IS-FORM is invoked
;;; on the (NULL (FIND (FOO) LIST)) form, a substitution very similar
;;; to the previous ones is made: (TEMP0 (FIND (FOO) LIST) (FIND (FOO)
;;; LIST) NIL). Next, SUBSTITUTE-IS-FORM is invoked on the NEW-FORM of
;;; that substitution, (FIND (FOO) LIST), and the returned form
;;; _replaces_ NEW-FORM, while the returned additional substitutions
;;; are prepended to the list of substitutions made (there was only
;;; one). The original form is then rewritten as
;;;
;;;   (let* ((temp1 (foo)) (temp2 list) (temp0 (find temp1 temp2)))
;;;          (null temp0))
;;;
;;; and all three TEMP* variables are reported in the OUTCOME.
(defmethod substitute-is-list-form ((first (eql 'null)) form env)
  (declare (ignore form))
  (multiple-value-bind (new-form subs) (call-next-method)
    (values new-form (substitute-in-subs subs env))))

(defmethod substitute-is-list-form ((first (eql 'endp)) form env)
  (declare (ignore form))
  (multiple-value-bind (new-form subs) (call-next-method)
    (values new-form (substitute-in-subs subs env))))

(defun substitute-in-subs (subs env)
  (append (mapcan (lambda (sub)
                    (multiple-value-bind (new-form new-subs)
                        (substitute-is-form (sub-new-form sub) env)
                      (setf (sub-new-form sub) new-form)
                      new-subs))
                  subs)
          subs))

;;; NOT is not like NULL and ENDP if its argument can be NIL or T
;;; only, which is not very interesting. Thus, we do substitution in
;;; its argument form but leave the value of its argument untouched.
(defmethod substitute-is-list-form ((first (eql 'not)) form env)
  (when (= (length form) 2)
    (on-values (substitute-is-form (second form) env)
      `(not ,*))))
