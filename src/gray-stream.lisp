(in-package :try)

(defclass wrapped-stream (trivial-gray-streams:fundamental-stream)
  ((inner-stream :initarg :stream :reader inner-stream)))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (inner-stream stream)))

(defmethod close ((stream wrapped-stream) &key abort)
  (close (inner-stream stream) :abort abort))


(defclass line/column-character-output-stream
    (wrapped-stream trivial-gray-streams:fundamental-character-output-stream)
  ((line :initform 0 :accessor line)
   (column :accessor column)))

(defmethod initialize-instance :after
    ((stream line/column-character-output-stream) &key &allow-other-keys)
  (setf (column stream) (or (stream-line-column* (inner-stream stream))
                            0)))

(defmethod trivial-gray-streams:stream-line-column
    ((stream line/column-character-output-stream))
  (with-slots (inner-stream column) stream
    ;; If INNER-STREAM is known to not be at COLUMN, then something
    ;; was written to it not through STREAM (see initialization of
    ;; COLUMN above). In that case, just force a new line to sync up
    ;; again.
    (setf column (sync-stream-line-column column inner-stream))
    column))

(defun sync-stream-line-column (to-column stream)
  (let ((syncedp nil))
    (labels
        ((sync (stream)
           (let ((column (stream-line-column* stream)))
             (cond (column
                    ;; https://gitlab.common-lisp.net/cmucl/cmucl/-/issues/121
                    (when (and (/= column to-column)
                               #+cmucl
                               (not (typep
                                     stream
                                     'lisp::fill-pointer-output-stream)))
                      (terpri stream)
                      (assert (zerop (stream-line-column* stream)))
                      (setq syncedp t)))
                   (t
                    (typecase stream
                      (broadcast-stream
                       (mapc #'sync (broadcast-stream-streams stream)))
                      (echo-stream
                       (sync (echo-stream-output-stream stream)))
                      (synonym-stream
                       (sync (symbol-value (synonym-stream-symbol stream))))
                      (two-way-stream
                       (sync (two-way-stream-output-stream stream)))))))))
      (sync stream)
      (cond (syncedp
             ;; In case there is BROADCAST-STREAM in there, sync the
             ;; rest of the streams to column 0.
             (setq to-column 0)
             (sync stream)
             0)
            (t
             to-column)))))

(defun stream-line-column* (stream)
  #+allegro
  (excl:charpos stream)
  #+ccl
  (ccl:stream-line-column stream)
  #+clisp
  (sys::line-position stream)
  #+cmucl
  (lisp::charpos stream)
  #+ecl
  (si:file-column stream)
  #+sbcl
  (sb-kernel:charpos stream)
  #-(or allegro ccl clisp cmucl ecl sbcl)
  (typecase stream
    (impl-specific-gray:fundamental-stream
     (trivial-gray-streams:stream-line-column stream))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream line/column-character-output-stream) char)
  (with-slots (inner-stream line column) stream
    (write-char char inner-stream)
    (cond ((char= char #\Newline)
           (incf line)
           (setf column 0))
          (t
           (incf column)))))

(defgeneric stream-line (stream))

(defmethod stream-line ((stream line/column-character-output-stream))
  (line stream))

(defgeneric stream-column (stream))

(defmethod stream-column ((stream line/column-character-output-stream))
  (column stream))
