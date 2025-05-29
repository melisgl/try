(in-package :try-test)

(defvar *in-with-std-try* nil)

(defmacro with-std-try (&body body)
  `(try::with-bindings (unless *in-with-std-try*
                         `((*in-with-std-try* t)
                           (*package* ,(find-package :try-test))
                           (*print-right-margin* 80)
                           (*print-compactly* nil)
                           (*defer-describe* nil)
                           (*print-parent* t)
                           (*categories* ,(test-categories))))
     ,@body))

(defun test-categories ()
  '((abort*             :marker "!")
    (unexpected-failure :marker "×")
    (unexpected-success :marker "✓×")
    (skip               :marker "-")
    (expected-failure   :marker "××")
    (expected-success   :marker "✓")))

(defmacro check-try-output ((testable &key (print ''(or outcome error*))
                             (describe ''unexpected)
                             (collect ''unexpected)
                             (debug nil)
                             (rerun ''unexpected)
                             (replay-events nil)
                             (printer ''tree-printer)
                             use-debug-io
                             (expected-outcome
                              (if (alexandria:featurep '(or :abcl :clisp))
                                  ''(or failure success)
                                  ''success))
                             msg)
                            expected)
  `(%check-try-output ,testable ,expected
                      :print ,print :describe ,describe
                      :collect ,collect
                      :debug ,debug :rerun ,rerun
                      :replay-events ,replay-events
                      :printer ,printer
                      :use-debug-io ,use-debug-io
                      :expected-outcome ,expected-outcome
                      :msg ,msg))

(defun %check-try-output (testable expected &key (print 'outcome)
                          (describe 'unexpected) (collect 'unexpected)
                          (debug nil) (rerun 'unexpected)
                          (replay-events nil)
                          (printer 'tree-printer) use-debug-io
                          (expected-outcome
                           (if (alexandria:featurep '(or :abcl :clisp))
                               '(or failure success)
                               'success))
                          msg)
  (flet ((call-it (stream)
           (with-std-try
             (if replay-events
                 (replay-events testable :print print :describe describe
                                :collect collect
                                :stream stream :printer printer)
                 (try testable :print print :describe describe
                      :collect collect :debug debug :rerun rerun
                      :stream stream :printer printer)))))
    (let* ((trial nil)
           (output (let ((*print-duration* nil))
                     (if use-debug-io
                         (with-output-to-string (*debug-io*)
                           (setq trial (call-it *debug-io*)))
                         (with-output-to-string (stream)
                           (setq trial (call-it stream)))))))
      (with-expected-outcome (expected-outcome)
        (is (null (mismatch% output expected :max-suffix-length 40))
            :msg (or msg (list "TRY output matches what's expected."))))
      trial)))

(defmacro check-implicit-try-output ((form &key (print ''outcome)
                                      (describe ''unexpected)
                                      (collect ''unexpected)
                                      (debug nil)
                                      (rerun ''unexpected)
                                      (printer ''tree-printer)
                                      (expected-outcome
                                       (if (alexandria:featurep :clisp)
                                           ''(or failure success)
                                           ''success))
                                      msg)
                                     expected)
  (with-gensyms (trial output stream)
    `(let* ((,trial nil)
            (,output (with-std-try
                       (with-new-implicit-try
                         (let ((*print* ,print)
                               (*describe* ,describe)
                               (*collect* ,collect)
                               (*debug* ,debug)
                               (*rerun* ,rerun)
                               (*printer* ,printer)
                               (*print-duration* nil))
                           (with-output-to-string (,stream)
                             (let ((*stream* ,stream))
                               (setq ,trial ,form))))))))
       (with-expected-outcome (,expected-outcome)
         (is (null (mismatch% ,output (identity ,expected)))
             :msg (or ,msg
                      (list
                       "TRY/IMPLICIT output matches what's expected."))))
       ,trial)))

(defun check-outcomes (fn counts &key (print nil))
  (let ((trial (try fn :print print)))
    (is (endp (different-elements (counts-to-list (list-total-outcomes trial))
                                  (counts-to-list (to-counts counts))
                                  :pred #'equal)))))

(defun counts-to-list (counts)
  (loop for category in *categories*
        for count in counts
        collect (list (first category) count)))

(defun to-counts (x)
  (let ((counts (try::counter-counts (try::make-counter *categories*))))
    (loop for (type n) in x
          do (let ((i (position type *categories* :key #'first
                                :test #'equal)))
               (assert i)
               (incf (aref counts i) n)))
    (coerce counts 'list)))

(defun list-total-outcomes (trial)
  (coerce (try::counter-counts (try::counter trial)) 'list))

(defmacro %with-deftest-registry (&body body)
  `(let ((*deftest-registry* (make-hash-table)))
     (declare (special *deftest-registry*))
     ,@body))

(defmacro %with-deftest (((var name) args &body deftest-body) &body body)
  `(let ((,var (read-from-string ,name)))
     (eval (list 'deftest ,var ,args ,@deftest-body))
     (unwind-protect
          (progn ,@body)
       ;; This is to avoid a warning from AllegroCL.
       (when (symbol-package ,var)
         (unintern ,var (symbol-package ,var))))))

(defmacro call-deftest ((name args &body body) &rest call-args)
  `(progn
     ;; Muffle redefinition warning.
     (handler-bind ((warning #'muffle-warning))
       (eval `(deftest ,',name ,',args ,'(progn ,@body))))
     ;; This is to avoid a undefined function warnings on AllegroCL
     ;; and CCL.
     (funcall (identity ',name) ,@call-args)))

(defmacro named-lambda-test (name args &body body)
  `(progn
     ;; Muffle redefinition warning.
     (handler-bind ((warning #'muffle-warning))
       (eval `(deftest ,',name ,',args ,'(progn ,@body))))
     ',name))

(defmacro with-new-implicit-try (&body body)
  `(let ((try::*try-id* nil)
         (try::*record-event* nil)
         (try::*trial* nil)
         (try::*expected-outcome* 'success)
         (try::*skip* nil))
     ,@body))

(defmacro with-silent-implicit-try (&body body)
  `(with-new-implicit-try
     (let ((*print* nil)
           (*debug* nil))
       ,@body)))

(defmacro is-ctx-captures (expected &body body)
  `(signals (outcome :pred (lambda (c)
                             (check-ctx-captures c ,expected)))
     ,@body))

(defun check-ctx-captures (c expected)
  (and (is (endp (different-elements (try::finalize-captures (try::captures c))
                                     expected
                                     :pred #'equal)))
       (check-no-% c)))

(defun check-no-% (condition)
  (let ((printed-condition (try::%describe-condition-for-matching condition)))
    (is (null (search "%" printed-condition)))))
