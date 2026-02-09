(in-package :try)

(define-condition result (leaf outcome)
  ((check :initarg :check :reader check)
   (elapsed-seconds :initform nil :initarg :elapsed-seconds
                    :reader elapsed-seconds)
   (msg :initform nil :initarg :msg :reader msg)
   (ctx :initform nil :initarg :ctx :reader ctx)
   (captures :initform () :initarg :captures :reader captures)
   (print-captures :initform t :initarg :print-captures
                   :reader print-captures)
   (print-form-memoization-data :initform nil
                                :accessor print-form-memoization-data)))

(define-combi-event (expected result success))
(define-combi-event (unexpected result success))
(define-combi-event (expected result failure))
(define-combi-event (unexpected result failure))
(define-combi-event (result skip))
(define-condition result-abort* (result abort* dismissal) ())

;;; KLUDGE: For SUBTYPEP to work in DETERMINE-OUTCOME-TYPE. Not to be
;;; exported because for example EXPECTED-RESULT-SUCCESS is not a
;;; subtype of RESULT-SUCCESS. Note that with (DEFTYPE RESULT-SUCCESS
;;; '(AND RESULT SUCCESS)), we'd have another problem: (SUBTYPEP
;;; 'RESULT-SUCCESS 'SUCCESS) => NIL.
(define-combi-event (result success))
(define-combi-event (result failure))


(defmethod write-event ((result result) stream &key terse ctx)
  (%write-result result stream :terse terse :ctx ctx))

(defun %write-result (result stream &key terse ctx)
  (setf (print-form-memoization-data result)
        (or (print-form-memoization-data result) (make-hash-table)))
  (pprint-logical-block (stream nil)
    (%write-result-msg result stream :terse terse)
    (when ctx
      (%write-result-captures result stream)
      (%write-result-ctx result stream))))

(declaim (ftype function capture-subform capture-explicitp))

;;; Remove % and %% (see CAPTURE) in explicit captures, so that they
;;; don't pollute the form displayed in the events.
(defun %frob-form-for-printing (result form)
  ;; Memoize the results so that printing a result twice writes EQ
  ;; objects and *PRINT-CIRCLE* thus works.
  (let ((data (print-form-memoization-data result)))
    (labels ((explicitly-captured-p (subform)
               (loop for capture in (captures result)
                       thereis (and (capture-explicitp capture)
                                    (eq subform (capture-subform capture)))))
             (frob (form)
               (or (gethash form data)
                   (cond
                     ((atom form)
                      form)
                     ((and (member (car form) '(% %%))
                           ;; See if it's a proper list of length 2.
                           (consp (cdr form))
                           (null (cddr form))
                           (explicitly-captured-p (second form)))
                      (let* ((subform (second form))
                             (frobbed (frob subform)))
                        (setf (gethash subform data) frobbed)
                        (values frobbed t)))
                     (t
                      (let ((new (cons nil nil)))
                        (setf (gethash form data) new)
                        (multiple-value-bind (car car-frobbed?)
                            (frob (car form))
                          (multiple-value-bind (cdr cdr-frobbed?)
                              (frob (cdr form))
                            (cond ((or car-frobbed? cdr-frobbed?)
                                   (setf (car new) car
                                         (cdr new) cdr)
                                   (values new t))
                                  (t
                                   (setf (gethash form data) form)))))))))))
      (frob form))))

(defun %write-result-msg (result stream &key terse)
  (cond (terse
         (%%write-result-msg result stream))
        (t
         (format stream "~@<~S in check:~:@_~:@>"
                 (event-category result *categories*))
         (pprint-logical-block (stream nil :per-line-prefix "  ")
           (%%write-result-msg result stream)))))

(defun %%write-result-msg (result stream)
  (destructuring-bind (&optional msg-control &rest msg-args)
      (alexandria:ensure-list (msg result))
    (if msg-control
        (apply #'format stream
               (concatenate 'string "~@<"
                            (substitute-tilde-percent msg-control)
                            "~:@>")
               msg-args)
        (format stream "~@<~S~:@>"
                (%frob-form-for-printing result (check result))))))

(declaim (ftype function finalize-captures))

(defun %write-result-captures (result stream)
  (when (print-captures result)
    (let ((captures (finalize-captures (captures result))))
      (when captures
        (format stream "~:@_where~:@_")
        ;; Note that we do not enforce indentation with
        ;; :PER-LINE-PREFIX. That would be confusing e.g. when
        ;; multiline string values are printed.
        (pprint-logical-block (stream nil)
          (loop
            for capture in captures
            for i upfrom 0
            do (when (plusp i)
                 (format stream "~:@_"))
               (destructuring-bind (subform value valuesp explicitp) capture
                 (declare (ignore explicitp))
                 (let ((subform (%frob-form-for-printing result subform)))
                   (if valuesp
                       (pprint-logical-block (stream nil)
                         (format stream "  ~S == " subform)
                         (pprint-logical-block (stream nil)
                           (format stream "~{~S~^~:@_~}" value)))
                       (format stream "~@<  ~S = ~S~:@>"
                               subform value))
                   (when (and (stringp value) (find #\Newline value))
                     (format stream "~:@_"))))))))))

(defun %write-result-ctx (result stream)
  (destructuring-bind (&optional ctx-control &rest ctx-args) (ctx result)
    (when ctx-control
      (apply #'format stream
             (concatenate 'string "~:@_~@<"
                          (substitute-tilde-percent ctx-control)
                          "~:@>")
             ctx-args))))

;;; To be used in logical blocks, substitute ~% with ~@:_
;;; (PPRINT-NEWLINE :MANDATORY). FIXME: This does not handle literal
;;; newlines, ~~, nor recursion via ~?.
(defun substitute-tilde-percent (string)
  (with-output-to-string (s)
    (let ((n (length string))
          (start 0))
      (loop for pos = (position #\~ string :start start)
            while pos
            do (cond ((and (< (1+ pos) n)
                           (char= (aref string (1+ pos)) #\%))
                      (write-string string s :start start :end pos)
                      (write-string "~@:_" s)
                      (setq start (+ pos 2)))
                     (t
                      (write-string string s :start start :end (1+ pos))
                      (setq start (+ pos 1))))
            finally (write-string string s :start start)))))
