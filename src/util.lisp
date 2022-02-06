(in-package :try)

(in-readtable pythonic-string-syntax)

;;;; Macro writing

(defun make-gensym (object)
  (if (symbolp object)
      (gensym (symbol-name object))
      (gensym object)))

(defmacro with-gensyms (vars &body body)
  `(let ,(mapcar #'(lambda (v) `(,v (make-gensym ',v)))
          vars)
     ,@body))

(defun intern-hyphenated (symbols)
  (assert symbols)
  (intern (format nil "~{~A~^-~}" symbols) (symbol-package (first symbols))))

(defun present-body (body)
  (if (and (listp body)
           (= (length body) 1))
      (first body)
      `(progn ,@body)))

;;; See TEST-ENSURE-REST-ARG.
(defun ensure-rest-arg (lambda-list &optional (rest-var (make-gensym 'rest)))
  (let ((rest (member '&rest lambda-list)))
    (if rest
        ;; Return the name of the &REST argument.
        (values lambda-list (second rest))
        ;; Let's insert a &REST argument. It must be after any
        ;; &OPTIONAL but before &KEY and &AUX. Since &KEY and &AUX
        ;; must be after any optional args anyway, let's just put it
        ;; before either or at the end of the lambda list.
        (let ((position (position-if (lambda (arg)
                                       (member arg '(&key &aux)))
                                     lambda-list)))
          (values (if position
                      (append (subseq lambda-list 0 position)
                              (list '&rest rest-var)
                              (subseq lambda-list position))
                      (append lambda-list (list '&rest rest-var)))
                  rest-var)))))

;;; Return a form that (when evaluated in a function with LAMBDA-LIST)
;;; evaluates to the actual arglist of the function. See
;;; TEST-LAMBDA-LIST-WITH-&REST-TO-ARGLIST-FORM.
(defun lambda-list-with-&rest-to-arglist-form (lambda-list)
  (let ((rest-var (second (member '&rest lambda-list))))
    (assert rest-var () "No ~S found in ~S." '&rest lambda-list)
    `(append (list ,@(loop for element in lambda-list
                           until (eq element '&rest)
                           unless (eq element '&optional)
                             collect (if (atom element)
                                         element
                                         ;; &OPTIONAL (O 0)
                                         (first element))))
             ,rest-var)))

(defun lambda-list-to-arglist-form (lambda-list)
  (let ((lambda-list (ensure-rest-arg lambda-list 'rest)))
    (values (lambda-list-with-&rest-to-arglist-form lambda-list)
            lambda-list)))


;;;; Special variables

;;; As in LET, later bindings have precedence.
(defmacro with-bindings (bindings &body body)
  (with-gensyms (%bindings)
    `(let ((,%bindings ,bindings))
       (progv (mapcar #'first ,%bindings) (mapcar #'second ,%bindings)
         ,@body))))


;;;; Printing

;;; Execute BODY via a dummy PRINT-OBJECT method to force circularity
;;; detection.
(defmacro with-circularity-detection ((stream) &body body)
  (alexandria:with-gensyms (values dummy)
    `(let* ((,values nil)
            (,dummy (make-instance
                     'dummy-with-print-object
                     :fn (lambda (,stream)
                           (setq ,values
                                 (multiple-value-list ,@body))))))
       (prin1 ,dummy ,stream)
       (values-list ,values))))

(defclass dummy-with-print-object ()
  ((fn :initarg :fn)))

(defmethod print-object ((dummy dummy-with-print-object) stream)
  (funcall (slot-value dummy 'fn) stream))


;;;; Types

(defun valid-type-specifier-p (type)
  (handler-case
      (null (nth-value 1 (ignore-errors (typep nil type))))
    ;; Silence compiler notes on SBCL when run via ASDF:TEST-SYSTEM.
    #+sbcl
    (sb-kernel:parse-unknown-type ())
    #+cmucl
    (sys::parse-unknown-type ())))

(deftype valid-type-specifier () '(satisfies valid-type-specifier-p))

;;; Returns NIL for invalid type specifiers.
(defun safe-typep (object type)
  (values (ignore-errors (typep object type))))

;;; Returns NIL for invalid type specifiers.
(defun safe-subtypep (type1 type2)
  (values (ignore-errors (subtypep type1 type2))))


;;;; Sequences

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defun subseq* (seq start end)
  (subseq seq (max start 0)
          (if end
              (min end (length seq))
              nil)))

(defun shuffle (seq)
  "Copy of SEQ and shuffle it using Fisher-Yates algorithm."
  (if (listp seq)
      (coerce (shuffle-vector! (coerce seq 'vector)) 'list)
      (shuffle-vector! (copy-seq seq))))

(defun shuffle! (seq)
  "Shuffle SEQ using Fisher-Yates algorithm."
  (if (listp seq)
      (coerce (shuffle-vector! (coerce seq 'vector)) 'list)
      (shuffle-vector! seq)))

(defun shuffle-vector! (vector)
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)


;;;; Multiple values

(defmacro on-values (form &body body)
  """ON-VALUES evaluates FORM and transforms its return values one by
  one based on forms in BODY. The Nth value is replaced by the return
  value of the Nth form of BODY evaluated with [`*`][dislocated] bound
  to the Nth value. If the number of values exceeds the number of
  transformation forms in BODY then the excess values are returned as
  is.

  ```cl-transcript
  (on-values (values 1 "abc" 7)
    (1+ *)
    (length *))
  => 2
  => 3
  => 7
  ```

  If the number of values is less than the number of transformation
  forms, then in later transformation forms [`*`][dislocated] is bound
  to NIL.

  ```cl-transcript
  (on-values (values)
    *
    *)
  => NIL
  => NIL
  ```

  The first forms in BODY may be options. Options must precede
  transformation forms. With :TRUNCATE T, the excess values are
  discarded.

  ```cl-transcript
  (on-values (values 1 "abc" 7)
    (:truncate t)
    (1+ *)
    (length *))
  => 2
  => 3
  ```

  The :ON-LENGTH-MISMATCH option may be NIL or a function of a single
  argument. If the number of values and the number of transformation
  forms is different, then this function is called to transform the
  list of values. :TRUNCATE is handled before :ON-LENGTH-MISMATCH.

  ```cl-transcript
  (on-values 1
    (:on-length-mismatch (lambda (values)
                           (if (= (length values) 1)
                               (append values '("abc"))
                               values)))
    (1+ *)
    *)
  => 2
  => "abc"
  ```

  If the same option is specified multiple times, only the first one
  is in effect.
  """
  (multiple-value-bind (options preds)
      (%extract-options '(:on-length-mismatch :truncate) body :options-first t)
    (let ((n-preds (length preds)))
      (destructuring-bind (&optional &key on-length-mismatch)
          (find :on-length-mismatch options :key #'first)
        (destructuring-bind (&optional &key truncate)
            (find :truncate options :key #'first)
          (with-gensyms (values)
            `(let ((,values (multiple-value-list ,form)))
               (block nil
                 ,@(when truncate
                     `((when (and ,truncate (< ,n-preds (length ,values)))
                         (setq ,values (subseq ,values 0 ,n-preds)))))
                 ,@(when on-length-mismatch
                     `((when (/= (length ,values) ,n-preds)
                         (setq ,values (funcall ,on-length-mismatch ,values)))))
                 (values-list
                  (append (list ,@(loop for i upfrom 0
                                        for pred in preds
                                        collect `(let ((* (pop ,values)))
                                                   ,pred)))
                          ,values))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %extract-options (option-names body &key options-first)
    (loop for clause in body
          when (and (listp clause)
                    (member (first clause) option-names))
            when (and options-first clauses)
              do (error "Options must come first.")
          end
          and collect clause into options
          else
            collect clause into clauses
          end
          finally (return (values options clauses)))))


;;;; Misc

(defun bool= (a b)
  (eq (not a) (not b)))


;;;; Compiler stuff

#+sbcl
(defun compiler-policy (env)
  (sb-c::policy-to-decl-spec
   (sb-c::lexenv-policy env)
   ;; Take into account restrictions imposed by
   ;; SB-EXT:RESTRICT-COMPILER-POLICY (used by Slime.)
   nil))

#-sbcl
(defun compiler-policy (env)
  (declare (ignore env))
  '((compilation-speed 1) (debug 1) (safety 1) (space 1) (speed 1)))

(defun compiler-policy-quantity (quantity env)
  (second (assoc quantity (compiler-policy env))))

(defvar *dummy*)
(defmacro without-tail-call (&body body)
  `(multiple-value-prog1
       (progn ,@body)
     (setq *dummy* t)))


;;;; Non-local transfer of control

(defmacro %unwind-protect (protected &body cleanup)
  `(unwind-protect
        ,protected
     (unless *internal-error*
       ,@cleanup)))

(defmacro on-finish (protected &body cleanup)
  ;; &BODY is just an indentation hint.
  (assert (= (length cleanup) 1))
  (with-gensyms (finishedp)
    `(let ((,finishedp nil))
       (%unwind-protect
           (multiple-value-prog1
               ,protected
             (setq ,finishedp t))
         (funcall ,(first cleanup) ,finishedp)))))

(defmacro on-nlx (protected &body cleanup)
  (with-gensyms (finishedp)
    `(on-finish ,protected
       (lambda (,finishedp)
         (unless ,finishedp
           ,@cleanup)))))

(defmacro on-return (protected &body cleanup)
  `(multiple-value-prog1
       ,protected
     ,@cleanup))

(defmacro with-retry/catch ((&key catch on-retry) &body body)
  `(block nil
     (loop
       ;; KLUDGE: Clisp complain about illegal syntax without the
       ;; PROGN.
       (progn
         (catch ,catch
           (return (progn ,@body)))
         ,on-retry))))

;;;; Same thing with TAGBODY and GO instead of CATCH and THROW.
(defmacro with-retry ((&key (retry 'retry) on-retry) &body body)
  (cond ((and retry on-retry)
         (with-gensyms (body-tag)
           `(block ,retry
              (tagbody
                 (go ,body-tag)
                 ,retry
                 ,on-retry
                 ,body-tag
                 (macrolet ((,retry () (list 'go ',retry)))
                   (return-from ,retry
                     (progn ,@body)))))))
        (retry
         `(block ,retry
            (tagbody
               ,retry
               (macrolet ((,retry () (list 'go ',retry)))
                 (return-from ,retry
                   (progn ,@body))))))
        (t
         `(block nil
            ,@body))))


;;;; Restarts

;;; Like CL:RESTART-CASE (it's being SHADOWed) but takes forms instead
;;; of functions for :INTERACTIVE, :REPORT and :TEST, which keeps
;;; things more concise.
(defmacro restart-case (form (&key stream condition)
                        &body bindings)
  (let ((stream (or stream (make-gensym 'stream)))
        (condition (or condition (make-gensym 'condition))))
    `(cl:restart-case
         ,form
       ,@(loop for binding in bindings
               collect (%wrap-restart-case-binding-in-lambda
                        binding stream condition)))))

;;; A binding in its full form shall look like this:
;;;
;;;     (<RESTART-NAME> <LAMBDA-LIST> :interactive (lambda () ...)
;;;       :report (lambda (stream) ...) :test (lambda (condition) ...)
;;;       <BODY>)
;;;
;;; In the BINDING argument, instead of LAMBDAs we have a single form.
(defun %wrap-restart-case-binding-in-lambda (binding stream-var condition-var)
  (loop for rest on (cddr binding) by #'cddr
        for (option? form?) = rest
        while (member option? '(:interactive :report :test))
        append (%wrap-restart-case-option-in-lambda
                option? form? stream-var condition-var)
          into options
        finally (return (append (subseq binding 0 2) options rest))))

(defun %wrap-restart-case-option-in-lambda (option form stream-var
                                            condition-var)
  (ecase option
    (:interactive `(:interactive (lambda () ,form)))
    (:report `(:report (lambda (,stream-var)
                         (declare (ignorable ,stream-var))
                         (let ((*package* (find-package :keyword)))
                           ,(if (stringp form)
                                `(write-string ,form ,stream-var)
                                form)))))
    (:test `(:test (lambda (,condition-var)
                     (declare (ignorable ,condition-var))
                     ,form)))))

;;; Like CL:RESTART-CASE (it's being SHADOWed) but takes forms instead
;;; of functions for :INTERACTIVE, :REPORT and :TEST, which keeps
;;; things more concise.
(defmacro restart-bind (((&key stream condition) &rest bindings)
                        &body body)
  (let ((stream (or stream (make-gensym 'stream)))
        (condition (or condition (make-gensym 'condition))))
    `(cl:restart-bind
         ,(loop for binding in bindings
                collect (%wrap-restart-bind-binding-in-lambda
                         binding stream condition))
       ,@body)))

;;; Return (<RESTART-NAME> (lambda (...) ...) :interactive-function
;;;          (lambda () ...) :report-function (lambda (<STREAM-VAR>)
;;;          ...) :report-function (lambda (<CONDITION-VAR>) ...))
(defun %wrap-restart-bind-binding-in-lambda (binding stream-var condition-var)
  (destructuring-bind (restart-name lambda-list &rest rest) binding
    (loop for rest on rest by #'cddr
          for (option? form?) = rest
          while (member option? '(:interactive :report :test))
          append (%wrap-restart-bind-option-in-lambda
                  option? form? stream-var condition-var)
            into options
          finally (return (append (list restart-name)
                                  `((lambda ,lambda-list ,@rest))
                                  options)))))

(defun %wrap-restart-bind-option-in-lambda (option form stream-var
                                            condition-var)
  (ecase option
    (:interactive `(:interactive-function (lambda () ,form)))
    (:report `(:report-function (lambda (,stream-var)
                                  (declare (ignorable ,stream-var))
                                  (let ((*package* (find-package :keyword)))
                                    ,(if (stringp form)
                                         `(write-string ,form ,stream-var)
                                         form)))))
    (:test `(:test-function (lambda (,condition-var)
                              (declare (ignorable ,condition-var))
                              ,form)))))


;;;; Debugger

(defmacro with-debugger-hook (fn &body body)
  (with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               (funcall ,fn ,condition)
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))

(defmacro with-debugger-hook-form (form &body body)
  (with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               ,form
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))


;;;; Time

(defvar *start-time*)

(defmacro with-timing (&body body)
  `(let ((*start-time* (get-internal-real-time)))
     ,@body))

(defvar *testing-timing* nil)

(defun get-elapsed-seconds ()
  (/ (cond ((eq *testing-timing* t)
            (* 2 (random internal-time-units-per-second)))
           ((numberp *testing-timing*)
            (* *testing-timing* internal-time-units-per-second))
           (t
            (- (get-internal-real-time) *start-time*)))
     internal-time-units-per-second))
