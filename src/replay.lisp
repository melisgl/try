(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @replay (:title "Reprocessing Trials")
  (replay-events function))

;;; This allows dressing up predicate functions as types.
(defvar *collect-satisfies-pred* nil)
(defun collect-satisfies-pred-p (event)
  (dbg "(collect-satisfies-pred-p ~S) => ~S"
       event (funcall *collect-satisfies-pred* event))
  (funcall *collect-satisfies-pred* event))

(defun replay-events (trial &key (collect *try-collect*)
                              (print *try-print*) (describe *try-describe*)
                              (stream *try-stream*) (printer *try-printer*))
  """REPLAY-EVENTS reprocesses the events [collected][@COLLECT] in
  TRIAL without actually running the tests that produced them. It
  simply signals the events collected in TRIAL again to allow further
  processing. It takes the same arguments as TRY except
  [DEBUG][argument], [COUNT][argument] and RERUN. The values of
  *CATEGORIES* and *COUNT* that were in effect for TRIAL are used, and
  their current values are ignored to be able to keep consistent
  counts (see @COUNT).

  Suppose we ran a large test using the default `:PRINT` and into a
  rare non-deterministic bug.

  ```cl-transcript (:dynenv try-transcript)
  (deftest some-test ()
    (with-test (inner)
      (is t)
      (is (= 10 7))   ; fake non-deterministic bug
      (is t))
    (error "my-msg"))

  (try 'some-test)
  ```

  Now, the output is too large with EXPECTED events and the backtrace
  cluttering it. We could try running the test again with different
  settings, or even just [rerunning][@rerun] it, but that might make
  the bug go away. Instead of searching for the interesting bits in
  the text output, we can replay the events and print only the
  UNEXPECTED events:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print-backtrace* nil))
    (replay-events ! :print 'unexpected))
  .. SOME-TEST
  ..   INNER
  ..     ⊠ (IS (= 10 7))
  ..   ⊠ INNER ⊠1 ⋅2
  ..   ⊟ "my-msg" (SIMPLE-ERROR)
  .. ⊟ SOME-TEST ⊟1 ⊠1 ⋅2
  ..
  ==> #<TRIAL (SOME-TEST) ABORT* 0.500s ⊟1 ⊠1 ⋅2>
  ```

  Now, we decide that nesting of test is unimportant here, change to
  new printer settings and replay:

  ```cl-transcript (:dynenv try-transcript)
  (let ((*print-backtrace* nil)
        (*print-parent* nil)
        (*print-compactly* t)
        (*defer-describe* t))
    (replay-events !))
  .. ⊠⊟
  .. ⊟ SOME-TEST ⊟1 ⊠1 ⋅2
  ..
  .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in SOME-TEST INNER:
  .. (IS (= 10 7))
  ..
  .. ;; UNHANDLED-ERROR (⊟) in SOME-TEST:
  .. "my-msg" (SIMPLE-ERROR)
  ..
  ==> #<TRIAL (SOME-TEST) ABORT* 0.500s ⊟1 ⊠1 ⋅2>
  ```
  """
  (try-default-unspecified-args :replayp t)
  (check-type trial trial)
  ;; This is an undocumented feature to allow filtering e.g by test
  ;; name.
  (let ((*collect-satisfies-pred*
          (when (functionp collect)
            (prog1 collect
              (setq collect '(satisfies collect-satisfies-pred-p))))))
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
            (finish-printing printer)))))))

(defun %replay-events (verdict)
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
    (%replay verdict)))

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
