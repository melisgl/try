(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @tests (:title "Tests")
  """In Try, tests are Lisp functions that record their execution in
  TRIAL objects. TRIALs are to tests what function call traces are to
  functions. In more detail, tests

  - create a TRIAL object and signal a TRIAL-START event upon entry
    to the function,

  - signal a VERDICT condition before returning normally or via a
    [non-local exit][clhs],

  - return the TRIAL object as the first value,

  - return explicitly returned values as the second, third, and so on
    values.

  See DEFTEST and WITH-TEST for more precise descriptions.
  """
  (deftest macro)
  (*run-deftest-when* variable)
  (test-bound-p function)
  (with-test macro)
  (list-package-tests function)
  (with-tests-run macro)
  (warn-on-tests-not-run macro)
  (@calling-test-functions section)
  #+nil
  (@debug section)
  (@print section)
  (@count section)
  (@collect section)
  (@rerun section)
  (@replay section))


(defmacro deftest (name lambda-list &body body &environment env)
  """DEFTEST is a wrapper around DEFUN to define global test functions.
  See DEFUN for a description of NAME, LAMBDA-LIST, and BODY. The
  behaviour common with WITH-TEST is described in @TESTS.

  ```cl-transcript (:dynenv try-transcript)
  (deftest my-test ()
    (write-string "hey"))
  => MY-TEST

  (test-bound-p 'my-test)
  => T

  (my-test)
  .. hey
  ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
  ```

  Although the common case is for tests to have no arguments, DEFTEST
  supports general function lambda lists. Within a global test,

  - NAME is bound to the TRIAL object

  - the first return value is the trial

  - values are not returned implicitly

  - values returned with an explicit RETURN-FROM are returned as
    values after the trial

  ```cl-transcript (:dynenv try-transcript)
  (deftest my-test ()
    (prin1 my-test)
    (return-from my-test (values 2 3)))

  (my-test)
  .. #<TRIAL (MY-TEST) RUNNING>
  ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
  => 2
  => 3
  ```
  """
  (multiple-value-bind (arglist-form lambda-list)
      (lambda-list-to-arglist-form lambda-list)
    (multiple-value-bind (body declarations doc)
        (alexandria:parse-body body :documentation t)
      (let ((trial name)
            (with-test-body (make-symbol (format nil "~S" 'deftest))))
        `(progn
           (defun ,name ,lambda-list
             ,@(when doc (list doc))
             ,@declarations
             (let ((,trial (make-trial ',name (cons ',name ,arglist-form))))
               ,(if (expand-with-trial-in-deftest-p env)
                    `(if *try-id*
                         (with-trial (,trial)
                           (wrap-trial-body-for-return ,name
                             ,@body))
                         (try/implicit ,trial))
                    `(flet ((,with-test-body (,trial)
                              (declare (ignorable ,trial))
                              (wrap-trial-body-for-return ,name
                                ,@body)))
                       (if *try-id*
                           ;; This is not in tail position so it will
                           ;; show up in the backtrace.
                           (let ((*call-test-fn* #',with-test-body)
                                 (*call-test* ,trial))
                             (call-test))
                           ;; This tail call may clobber the
                           ;; function's frame, but it calls the
                           ;; function again using :CFORM above.
                           (try/implicit ,trial))))))
           (register-deftest ',name)
           (eval-when (:compile-toplevel)
             (when (or (eq :compile-toplevel *run-deftest-when*)
                       (and (listp *run-deftest-when*)
                            (member :compile-toplevel *run-deftest-when*)))
               (invoke-test-interactively ',name ',lambda-list)))
           (eval-when (:load-toplevel)
             (when (or (eq :load-toplevel *run-deftest-when*)
                       (and (listp *run-deftest-when*)
                            (member :load-toplevel *run-deftest-when*)))
               (invoke-test-interactively ',name ',lambda-list)))
           (eval-when (:execute)
             (when (or (eq :execute *run-deftest-when*)
                       (and (listp *run-deftest-when*)
                            (member :execute *run-deftest-when*)))
               (invoke-test-interactively ',name ',lambda-list)))
           ',name)))))

;;; On SBCL, there seems to be one CTOR per for each MAKE-INSTANCE
;;; call compiled, so call MAKE-INSTANCE from a separate function to
;;; amortize the cost of the CTOR updating itself.
(declaim (notinline make-trial))
(defun make-trial (name cform &optional extra-initargs)
  (if extra-initargs
      (apply #'make-instance 'trial
             (append extra-initargs
                     `(%test-name ,name :cform ,cform)))
      (make-instance 'trial '%test-name name :cform cform)))

(defvar *run-deftest-when* nil
  "This may be any of :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL, :EXECUTE, or
  a list thereof. The value of *RUN-DEFTEST-WHEN* determines in what
  EVAL-WHEN situation to call the test function immediately after it
  has been defined with DEFTEST.

  For interactive development, it may be convenient to set it to
  :EXECUTE and have the test run when the DEFTEST is evaluated (maybe
  with Slime `C-M-x`, `slime-eval-defun`). Or set it to
  :COMPILE-TOPLEVEL, and have it rerun on Slime `C-c C-c`,
  `slime-compile-defun`.

  If the test has required arguments, an argument list is prompted for
  and read from *QUERY-IO*.")

(declaim (notinline call-test))
;;; These are really arguments, but we want to keep the backtrace
;;; clean.
(defvar *call-test-fn*)
(defvar *call-test*)
(defun call-test ()
  (let ((trial *call-test*))
    (with-trial (trial)
      (funcall *call-test-fn* trial))))

(defun invoke-test-interactively (name lambda-list)
  (let ((required-args (alexandria:parse-ordinary-lambda-list lambda-list)))
    (if required-args
        (eval `(,name ,@(read-argument-list name lambda-list)))
        (funcall name))))

(defun read-argument-list (name lambda-list)
  (format *query-io* "~&~@<Invoking test ~S ~S.
                      Enter a list of forms to be evaluated ~
                      and passed to the test (e.g. ~S):~:@>~%"
          name lambda-list '('hello (1+ 2)))
  (finish-output *query-io*)
  (read *query-io*))

;;; On SBCL, expanding WITH-TRIAL in DEFTEST gives us nicer backtraces
;;; and source locations. Without inlining, the backtrace consists of
;;; extra frames for CALL-TEST and WITH-TEST-BODY. The problem is that
;;; inlining bloats the code so much that compilation and execution of
;;; the test suite become twice as slow, not to speak of the memory
;;; consumption.
(defun expand-with-trial-in-deftest-p (env)
  #+nil
  (and env (<= (max (compiler-policy-quantity 'compilation-speed env)
                    (compiler-policy-quantity 'space env)
                    (compiler-policy-quantity 'speed env))
               (compiler-policy-quantity 'debug env)))
  (declare (ignore env))
  nil)


(defmacro with-test ((&optional var-or-name &key (name nil namep))
                     &body body)
  """Execute BODY in a [TRIAL][class] to group together CHECKs and
  other tests in its dynamic scope. BODY is executed in its lexical
  environment even on a rerun (see @RERUN).

  If VAR-OR-NAME is a non-`NIL` symbol, it is bound to the TRIAL
  object. NAME may be of any type, it is purely for presentation
  purposes. If NAME is not specified, then it defaults to VAR-OR-NAME.

  To facilitate returning values, a BLOCK is wrapped around BODY. The
  name of the block is VAR-OR-NAME if it is a symbol, else it's NIL.

  Both VAR-OR-NAME and NAME can be specified, but in this case VAR-OR-NAME
  must be a symbol:

  ```cl-transcript (:dynenv try-transcript)
  (with-test (some-feature :name "obscure feature")
    (prin1 some-feature)
    (is t)
    (return-from some-feature (values 1 2)))
  .. #<TRIAL (WITH-TEST ("obscure feature")) RUNNING>
  .. "obscure feature"
  ..   ⋅ (IS T)
  .. ⋅ "obscure feature" ⋅1
  ..
  ==> #<TRIAL (WITH-TEST ("obscure feature")) EXPECTED-SUCCESS 0.200s ⋅1>
  => 1
  => 2
  ```

  If only VAR-OR-NAME is specified:

  ```cl-transcript (:dynenv try-transcript)
  (with-test (some-feature)
    (prin1 some-feature)
    (is t)
    (return-from some-feature (values 1 2)))
  .. #<TRIAL (WITH-TEST (SOME-FEATURE)) RUNNING>
  .. SOME-FEATURE
  ..   ⋅ (IS T)
  .. ⋅ SOME-FEATURE ⋅1
  ..
  ==> #<TRIAL (WITH-TEST (SOME-FEATURE)) EXPECTED-SUCCESS 0.000s ⋅1>
  => 1
  => 2
  ```

  If neither is specified:

  ```cl-transcript (:dynenv try-transcript)
  (with-test ()
    (prin1 (current-trial))
    (is t)
    (return (values 1 2)))
  .. #<TRIAL (WITH-TEST (NIL)) RUNNING>
  .. NIL
  ..   ⋅ (IS T)
  .. ⋅ NIL ⋅1
  ..
  ==> #<TRIAL (WITH-TEST (NIL)) EXPECTED-SUCCESS 0.000s ⋅1>
  => 1
  => 2
  ```

  Finally, using that NAME defaults to VAR-OR-NAME and that it is
  valid to specify non-symbols for VAR-OR-NAME, one can also write:

  ```cl-transcript (:dynenv try-transcript)
  (with-test ("Some feature")
    (prin1 (current-trial))
    (is t)
    (return (values 1 2)))
  .. #<TRIAL (WITH-TEST ("Some feature")) RUNNING>
  .. "Some feature"
  ..   ⋅ (IS T)
  .. ⋅ "Some feature" ⋅1
  ..
  ==> #<TRIAL (WITH-TEST ("Some feature")) EXPECTED-SUCCESS 0.200s ⋅1>
  => 1
  => 2
  ```

  In summary and in contrast to DEFTEST, WITH-TEST

  - defines and runs a test at the same time,
  - the test function cannot have arguments,
  - may not bind their trial object to any variable,
  - may have a BLOCK named NIL,
  - has a NAME purely for presentation purposes.

  WITH-TEST can be thought of as analogous to `(FUNCALL (LAMBDA ()
  BODY))`. The presence of the LAMBDA is important because it is
  stored in the TRIAL object to support @RERUN.
  """
  ;; Muffle style warning about having both &OPTIONAL and &KEY.
  #+sbcl
  (declare (sb-ext:muffle-conditions style-warning))
  (multiple-value-bind (var name)
      (cond ((null var-or-name)
             (values (make-gensym '#:trial) name))
            ((symbolp var-or-name)
             (values var-or-name (if namep name `',var-or-name)))
            (t
             (when namep
               (error "~@<In ~S, ~A and non-symbol ~A arguments are ~
                      mutually exclusive.~:@>"
                      'with-test 'name 'var-or-name))
             (values (make-gensym '#:trial) var-or-name)))
    (let ((with-test-body (make-symbol
                           (if (symbol-package var)
                               (format nil "~S-~S" 'with-test var)
                               ;; VAR's name is not informative if
                               ;; uninterned. Keep the backtrace
                               ;; cleaner.
                               (format nil "~S" 'with-test)))))
      `(flet ((,with-test-body (,var)
                (declare (ignorable ,var))
                (wrap-trial-body-for-return ,var
                  ,@body)))
         (let ((,var (make-trial ,name
                                 ;; This is LAMBDA-TRIAL-P.
                                 (list #',with-test-body)
                                 *trial-initargs*)))
           (if *try-id*
               (let ((*call-test-fn* #',with-test-body)
                     (*call-test* ,var))
                 (call-test))
               ;; If not FINISHEDP, TRY does not rerun TRIAL (which
               ;; would result in it being skipped) but run it
               ;; normally. The following is equivalent to (FUNCALL
               ;; ,TRIAL) but avoids some checks.
               (try/implicit ,var)))))))

;;; A bypass for REPLAY-EVENTS.
(defvar *trial-initargs* ())

;;; Tested by TRY-TEST::TEST-TRIAL/RETURN.
(defmacro wrap-trial-body-for-return (trial &body body)
  (assert (symbolp trial))
  ;; Tail-call optimization can remove the frame of WITH-TEST-BODY and
  ;; leave us with uninformative CALL-TEST frames in the non-inlined
  ;; case.
  `(without-tail-call
     (block ,(if (symbol-package trial) trial nil)
       (let ()
         ,@body)
       (values))))


(defun deftest-registry-var-name (package)
  (unless (eq package #.(find-package :keyword))
    (let ((name (symbol-name '#:*deftest-registry*)))
      ;; This can fail, for example, when the package is locked.
      (ignore-errors (intern name package)))))

(defun get-deftest-registry (package)
  ;; For uninterned symbols, PACKAGE is NIL.
  (when package
    (let ((var-name (deftest-registry-var-name package)))
      (when (boundp var-name)
        (symbol-value var-name)))))

(defun ensure-deftest-registry (package)
  (let ((var-name (deftest-registry-var-name package)))
    (cond ((boundp var-name)
           (symbol-value var-name))
          (t
           (proclaim `(special ,var-name))
           (setf (symbol-value var-name) (make-hash-table))))))

(defun register-deftest (symbol)
  (let ((registry (ensure-deftest-registry (symbol-package symbol))))
    (when registry
      (setf (gethash symbol registry) (symbol-function symbol)))))

(defun test-bound-p (symbol)
  "See if SYMBOL names a global test (i.e. a test defined with
  DEFTEST). If since the execution of DEFTEST, the symbol has been
  UNINTERNed, FMAKUNBOUNDed, or redefined with DEFUN, then it no
  longer names a global test."
  (and (fboundp symbol)
       (let* ((package (symbol-package symbol))
              (registry (get-deftest-registry package)))
         (and registry
              (eq (symbol-function symbol)
                  (gethash symbol registry))))))

(defun list-package-tests (&optional (package *package*))
  "List all symbols in PACKAGE that name global tests in the sense of
  TEST-BOUND-P."
  (let ((registry (get-deftest-registry package)))
    (when registry
      (let ((r ()))
        (maphash (lambda (symbol function)
                   (if (or (null (symbol-package symbol))
                           (not (fboundp symbol))
                           (not (eq (symbol-function symbol) function)))
                       ;; It was UNINTERNed, FMAKUNBOUND, or redefined
                       ;; as a DEFUN.
                       (remhash symbol registry)
                       (push symbol r)))
                 registry)
        r))))

(defvar *tests-run*)

(defmacro with-tests-run ((tests-run) &body body)
  "Bind the symbol TESTS-RUN to an empty `EQ` hash table and execute
  BODY. The hash table reflects call counts to global tests. Keys are
  symbols naming global tests, and the values are the number of times
  the keys have been called."
  `(let* ((,tests-run (make-hash-table))
          (*tests-run* ,tests-run))
     ,@body))

(defun maybe-mark-test-run (trial)
  (when (and (boundp '*tests-run*) (deftest-trial-p trial))
    (incf (gethash (test-name trial) *tests-run* 0))))

(defmacro warn-on-tests-not-run ((&optional (package *package*)) &body body)
  "A convenience utility that records the global tests run by BODY
  with WITH-TESTS-RUN and, when BODY finishes, signals a warning for
  each global tests in PACKAGE not run.

  This is how Try runs its own tests:

  ```
  (defun test ()
    ;; Bind *PACKAGE* so that names of tests printed have package names,
    ;; and M-. works on them in Slime.
    (let ((*package* (find-package :common-lisp)))
      (warn-on-tests-not-run ((find-package :try))
        (print (try 'test-all
                    :print 'unexpected
                    :describe 'unexpected)))))
  ```"
  (alexandria:with-gensyms (tests-run test)
    `(with-tests-run (,tests-run)
       (%unwind-protect
           (progn ,@body)
         (dolist (,test (list-package-tests ,package))
           (unless (gethash ,test ,tests-run)
             (warn "~@<Test ~S not run.~:@>~%" ,test)))))))
