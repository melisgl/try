(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try/replay (:title "Reprocessing Trials")
  (replay-events function))

(defun replay-events (trial &key (collect *try-collect*)
                      (print *try-print*) (describe *try-describe*)
                      (stream *try-stream*) (printer *try-printer*))
  """REPLAY-EVENTS reprocesses the events collected (see @TRY/COLLECT)
  in TRIAL. It takes the same arguments as TRY except DEBUG, COUNT and
  RERUN. This is because REPLAY-EVENTS does not run any tests. It
  simply signals the events collected in TRIAL again to allow further
  processing. The values of *CATEGORIES* and *COUNT* that were in
  effect for TRIAL are used, and their current values are ignored to
  be able to keep consistent counts (see @TRY/COUNT).

  Suppose we have run a large test using the default `:PRINT 'LEAF`
  `:COLLECT 'UNEXPECTED` arguments for TRY, and now we have too much
  output to look at. Instead of searching for the interesting bits in
  the output, we can replay the events and print only the UNEXPECTED
  events:

  ```
  (replay-events ! :print 'unexpected)
  ```

  Or we could tell the printer to just print markers for *CATEGORIES*
  and :DESCRIBE at the end:

  ```
  (let ((*print-parent* nil)
        (*print-compactly* t)
        (*defer-describe* t)
        (*categories* (ascii-std-categories)))
    (replay-events !))
  .. ................F................!.....
  .. 
  .. ;; UNEXPECTED-FAILURE (F) in SOME-TEST INNER-TEST:
  .. (IS (= 5 6))
  .. debug info
  .. 
  .. ;; UNHANDLED-ERROR (!) in SOME-TEST:
  .. "my-msg" (MY-ERR)
  ```
  """
  (try-default-unspecified-args :replayp t)
  (check-type trial trial)
  (loop for (type arg-name) in
        `((,collect :collect) (,print :print) (,describe :describe))
        do (check-event-type type arg-name))
  (check-printer-arg printer)
  (with-try-context
    (let* ((*categories* (categories trial))
           (collector (make-%collector :count-type (count-of trial)
                                       :collect-type collect))
           (printer (make-instance printer :stream stream
                                   :print-type print
                                   :describe-type describe))
           (*record-event* nil))
      (handler-bind ((event
                       (lambda (event)
                         (when (eq *try-id* try-id)
                           (with-internal-errors
                             ;; See END-TRIAL.
                             (when *record-event*
                               (funcall *record-event* event))
                             (%count-and-collect-event collector event)
                             (%print-event printer event))))))
        (%unwind-protect
            (%replay-events (verdict trial))
          (finish-printing printer))))))

(defun %replay-events (trial)
  (labels
      ((%replay (child)
         ;; LEAFs in CHILDREN are resignalled as is, but TRIALs are
         ;; modified by :COLLECT, so TRIALs and VERDICTs, which have a
         ;; TRIAL slot, must be created anew.
         (if (typep child 'verdict)
             (let* ((trial (trial child))
                    (*trial-initargs* (%replay-trial-initargs trial)))
               (with-test (replay-trial :name (test-name trial))
                 (map nil #'%replay (reverse (children trial)))))
             (signal child))))
    (%replay trial)))

(defun %replay-trial-initargs (trial)
  (list :cform (cform trial)
        :elapsed-seconds (elapsed-seconds trial)
        :categories (categories trial)
        :count (count-of trial)
        :n-retries (n-retries trial)
        :how-to-end (how-it-ended trial)
        :has-non-collected-failed-child-p
        (has-non-collected-failed-child-p trial)
        :counter (copy-counter (non-collected-counter trial))
        :non-collected-counter (copy-counter (non-collected-counter trial))))
