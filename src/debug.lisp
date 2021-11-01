(in-package :try)

(defvar *dbg* nil)

#+nil
(setq *dbg*
      (lambda (format-control &rest format-args)
        (apply #'format *trace-output*
               (concatenate 'string "~&In~@< ~S(~S): "
                            format-control
                            "~:@>~%")
               (and *trial* (test-name *trial*))
               (and *trial* (how-to-end *trial*))
               format-args)))
#+nil
(setq *dbg* nil)


(defmacro dbg (format-control &rest format-args)
  `(when *dbg*
     (funcall *dbg* ,format-control ,@format-args)))

(defvar *internal-error* nil)

(define-condition try-internal-error (serious-condition)
  ((error :initform nil :initarg :error :reader nested-error))
  (:report (lambda (c stream)
             (format stream "~@<Internal error:~:@_~A~:@>" (nested-error c)))))

(defmacro with-internal-errors (&body body)
  `(handler-bind ((error (lambda (c)
                           (unless *internal-error*
                             #+nil
                             (invoke-debugger c)
                             (setq *internal-error*
                                   (make-condition 'try-internal-error
                                                   :error c))
                             (error *internal-error*)))))
     ,@body))

