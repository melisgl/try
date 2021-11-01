(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try/tests (:title "Tests")
  """In Try, tests are Lisp functions that record their execution in
  TRIAL objects. TRIALs are to tests what function call traces are to
  functions. In more detail, tests

  - create a TRIAL object and signal a TRIAL-START event upon entry to
    the function,

  - signal a VERDICT condition before returning normally or via a
    @NON-LOCAL-EXIT,

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
  (@try/implicit-try section)
  (@try/explicit-try section)
  #+nil
  (@try/debug section)
  (@try/print section)
  (@try/count section)
  (@try/collect section)
  (@try/rerun section)
  (@try/replay section))


(defmacro deftest (name lambda-list &body body &environment env)
  """DEFTEST is a wrapper around DEFUN to define global test functions.
  See DEFUN for a description of NAME, LAMBDA-LIST, and BODY. The
  behaviour common with WITH-TEST is described in @TRY/TESTS.

  ```
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

  ```
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
  (multiple-value-bind (args-form args)
      ;; Capture the actual args with which the function was called in
      ;; DEFTEST-ARGS.
      (lambda-list-to-arglist-form lambda-list)
    (multiple-value-bind (body declarations doc)
        (alexandria:parse-body body :documentation t)
      (let ((trial name)
            (with-test-body (make-symbol (format nil "~S" 'deftest))))
        `(progn
           (defun ,name ,args
             ,@(when doc (list doc))
             ,@declarations
             (let ((,trial (make-instance 'trial
                                          '%test-name ',name
                                          :cform (cons ',name ,args-form))))
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


(defmacro with-test ((&optional trial-var &key name) &body body)
  """Define a so-called lambda test to group together CHECKs and other
  tests it executes. WITH-TEST executes BODY in its lexical
  environment even on a rerun (see @TRY/RERUN).

  If TRIAL-VAR is a non-`NIL` symbol, bind it to the trial object.
  NAME may be any type, it is purely for presentation purposes. If
  NAME is NIL, then it defaults to TRIAL-VAR.

  To facilitate returning values, a BLOCK is wrapped around BODY. The
  name of the block is TRIAL-VAR if it is a symbol, else it's NIL.

  When both TRIAL-VAR and NAME are specified:

  ```
  (with-test (some-feature :name "obscure feature")
    (prin1 some-feature)
    (is t)
    (return-from some-feature (values 1 2)))
  .. #<TRIAL (WITH-TEST ("obscure feature")) RUNNING>
  .. obscure feature
  ..   ⋅ (IS T)
  .. ⋅ obscure feature ⋅1
  ..
  ==> #<TRIAL (WITH-TEST ("obscure feature")) EXPECTED-SUCCESS 0.000s ⋅1>
  => 1
  => 2
  ```

  If only TRIAL-VAR is specified:

  ```
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

  ```
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

  Finally, using that NAME defaults to TRIAL-VAR and that it is valid
  to specify non-symbols for TRIAL-VAR, one can also write:

  ```
  (with-test ("Some feature")
    (prin1 (current-trial))
    (is t)
    (return (values 1 2)))
  .. #<TRIAL (WITH-TEST ("Some feature")) RUNNING>
  .. Some feature
  ..   ⋅ (IS T)
  .. ⋅ Some feature ⋅1
  ..
  ==> #<TRIAL (WITH-TEST ("Some feature")) EXPECTED-SUCCESS 0.000s ⋅1>
  => 1
  => 2
  ```

  In summary and in contrast to global tests (those defined with
  DEFTEST), lambda tests

  - have no arguments,
  - are defined and called at the same time,
  - may not bind their trial object to any variable,
  - may have a BLOCK named NIL,
  - have a NAME purely for presentation purposes.

  Lambda tests can be thought of as analogous to `(FUNCALL (LAMBDA ()
  BODY))`. The presence of the LAMBDA is important because it is
  stored in the TRIAL object to support @TRY/RERUN.
  """
  ;; Muffle style warning about having both &OPTIONAL and &KEY.
  #+sbcl
  (declare (sb-ext:muffle-conditions style-warning))
  (multiple-value-bind (var name)
      (cond ((null trial-var)
             (values (make-gensym '#:trial) name))
            ((symbolp trial-var)
             (values trial-var (or name `',trial-var)))
            (t
             (values (make-gensym '#:trial) trial-var)))
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
         (let ((,var (apply #'make-instance 'trial
                            (append *trial-initargs*
                                    (list '%test-name ,name
                                          ;; This is LAMBDA-TRIAL-P.
                                          :cform (list #',with-test-body))))))
           (if *try-id*
               (let ((*call-test-fn* #',with-test-body)
                     (*call-test* ,var))
                 (call-test))
               ;; If not FINISHEDP, TRY does not rerun TRIAL (which
               ;; would result in it being skipped) but run it
               ;; normally. The following is equivalen to (FUNCALL
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
  (if (eq package (symbol-package :cl-user))
      ;; So that it's not BOUNDP.
      (gensym)
      (intern (symbol-name '#:*deftest-registry*) package)))

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
    (setf (gethash symbol registry) (symbol-function symbol))))

(defun test-bound-p (symbol)
  "See if SYMBOL names a global test (i.e. a test defined with
  DEFTEST). If since the execution of DEFTEST, the symbol has been
  uninterned, FMAKUNBOUND, or redefined with DEFUN, then it no longer
  names a global test."
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
  BODY. The has table reflects call counts to global tests. Keys are
  symbols naming global tests, and the values are the number of times
  the keys have been called."
  `(let* ((,tests-run (make-hash-table))
          (*tests-run* ,tests-run))
     ,@body))

(defun maybe-mark-test-run (trial)
  (when (and (boundp '*tests-run*) (named-trial-p trial))
    (incf (gethash (test-name trial) *tests-run* 0))))

(defmacro warn-on-tests-not-run ((&optional (package *package*)) &body body)
  "A convenience utility to that records the global tests run by BODY
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
