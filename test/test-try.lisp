(in-package :try-test)

(defun test-try ()
  (test-try/empty)
  (test-try/simple-success)
  (test-try/simple-unexpected-error)
  (test-try/printer-variable-bindings)
  (test-try/invalid-type)
  (test-try/print-outcome-type)
  (test-try/describe-outcome-type)
  (test-try/collect-and-replay)
  (test-try/collect-and-replay/timing)
  (test-try/debug-outcome-type)
  (test-try/debug-first-restart-continues)
  (test-try/unwind-from-verdict)
  (test-try/rerun)
  (test-try/count)
  (test-try/printer-variables)
  (test-try/random-crap-on-output-stream)
  (test-try/nested)
  (test-try/compact-printing)
  (test-try/compact-and-deferred-printing)
  (test-try/implicit))

(deftest test-try/empty ()
  (check-try-output (() :print 'result) "")
  (check-try-output (() :print 'outcome) "✓ (TRY NIL)
"))

(deftest %simple-success ()
  (is (= (1+ 1) 2)))

(deftest test-try/simple-success ()
  (check-try-output ('%simple-success)
                    "%SIMPLE-SUCCESS
  ✓ (IS (= (1+ 1) 2))
✓ %SIMPLE-SUCCESS ✓1
"))

(deftest %simple-unexpected-error ()
  (error 'my-err))

(deftest test-try/simple-unexpected-error ()
  (check-try-output ('%simple-unexpected-error :describe nil)
                    "%SIMPLE-UNEXPECTED-ERROR
  ! \"my-msg\" (MY-ERR)
! %SIMPLE-UNEXPECTED-ERROR !1
"))


(deftest test-try/printer-variable-bindings ()
  (let ((try::*event-print-bindings* '((*print-circle* t))))
    (check-try-output ('%simple-success :describe t)
                      "%SIMPLE-SUCCESS
  ✓ (IS (= #1=(1+ 1) 2))
    where
      #1# = 2
✓ %SIMPLE-SUCCESS ✓1
"))
  (let ((try::*event-print-bindings* '((*print-circle* nil))))
    (check-try-output ('%simple-success :describe t)
                      "%SIMPLE-SUCCESS
  ✓ (IS (= (1+ 1) 2))
    where
      (1+ 1) = 2
✓ %SIMPLE-SUCCESS ✓1
")))

(deftest test-try/invalid-type ()
  (with-failure-expected (#+abcl t #-abcl nil)
    (with-test (try)
      (signals (error :pred "valid type specifier")
        (try '%simple-success :debug '(invalid-type-specifier)
             :print nil))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :count '(invalid-type-specifier)
             :print nil))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :collect '(invalid-type-specifier)
             :print nil))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :rerun '(invalid-type-specifier)
             :print nil))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :print '(invalid-type-specifier)))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :describe '(invalid-type-specifier)
             :print nil)))
    (with-test (replay-events)
      (signals (error :pred "valid type specifier")
        (try '%simple-success :collect '(invalid-type-specifier)
             :print nil))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :print '(invalid-type-specifier)))
      (signals (error :pred "valid type specifier")
        (try '%simple-success :describe '(invalid-type-specifier)
             :print nil)))))

(deftest %all-check-outcome-types ()
  ;; EXPECTED-SUCCESS
  (is (eql (1+ 3) 4))
  ;; UNEXPECTED-FAILURE without captures
  (is (= 5 6) :ctx "debug info")
  (with-failure-expected ('failure)
    ;; UNEXPECTED-SUCCESS
    (is (equal 3 (1+ 2)))
    ;; EXPECTED-FAILURE
    (is nil :ctx "more info"))
  (with-skip ()
    (is t))
  ;; UNHANDLED-ERROR
  (error 'my-err))

(deftest test-try/print-outcome-type ()
  (check-try-output ('%all-check-outcome-types :print 'leaf :describe nil)
                    "%ALL-CHECK-OUTCOME-TYPES
  ✓ (IS (EQL (1+ 3) 4))
  × (IS (= 5 6))
  ✓× (IS (EQUAL 3 (1+ 2)))
  ×× (IS NIL)
  - (IS T)
  ! \"my-msg\" (MY-ERR)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")
  (check-try-output ('%all-check-outcome-types :print 'unexpected
                     :describe nil)
                    "%ALL-CHECK-OUTCOME-TYPES
  × (IS (= 5 6))
  ✓× (IS (EQUAL 3 (1+ 2)))
  ! \"my-msg\" (MY-ERR)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")
  (check-try-output ('%all-check-outcome-types
                     :print 'verdict
                     :describe nil
                     :expected-outcome 'success)
                    "! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")
  (check-try-output ('%all-check-outcome-types :print 'expected :describe nil)
                    "%ALL-CHECK-OUTCOME-TYPES
  ✓ (IS (EQL (1+ 3) 4))
  ×× (IS NIL)
  - (IS T)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
"))

(deftest test-try/describe-outcome-type ()
  (check-try-output ('%all-check-outcome-types
                     ;; Exclude UNHANDLED-ERROR because its backtrace
                     ;; is hard to check.
                     :describe '(and outcome (not unhandled-error)))
                    "%ALL-CHECK-OUTCOME-TYPES
  ✓ (IS (EQL #1=(1+ 3) 4))
    where
      #1# = 4
  × (IS (= 5 6))
    debug info
  ✓× (IS (EQUAL 3 #1=(1+ 2)))
     where
       #1# = 3
  ×× (IS NIL)
     more info
  - (IS T)
  ! \"my-msg\" (MY-ERR)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")
  (check-try-output ('%all-check-outcome-types
                     ;; Exclude UNHANDLED-ERROR again.
                     :describe '(and unexpected (not unhandled-error)))
                    "%ALL-CHECK-OUTCOME-TYPES
  ✓ (IS (EQL (1+ 3) 4))
  × (IS (= 5 6))
    debug info
  ✓× (IS (EQUAL 3 #1=(1+ 2)))
     where
       #1# = 3
  ×× (IS NIL)
  - (IS T)
  ! \"my-msg\" (MY-ERR)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")
  (is (< 200 (length (% (with-output-to-string (s)
                          (try '%all-check-outcome-types
                               :print 'unhandled-error
                               :describe 'unhandled-error
                               :stream s)))))))

(deftest %collect-test-case ()
  (%all-check-outcome-types)
  (with-test (failing-outer)
    (with-test (passing-inner)
      (is t)
      (is t))
    (is nil))
  (with-test (erroring-outer)
    (with-test (passing-inner)
      (is t)
      (is t))
    (error 'my-err)))

(deftest test-try/collect-and-replay (&key timing)
  (dolist (type '(t nil pass
                  outcome
                  result verdict error*
                  expected unexpected
                  success failure dismissal
                  ;; {EXPECTED, UNEXPECTED} X {SUCCESS, FAILURE,
                  ;; DISMISSAL}
                  expected-success expected-failure skip
                  unexpected-success unexpected-failure abort*
                  ;; TRIAL outcomes
                  verdict-skip verdict-abort*
                  ;; RESULT outcomes
                  result-skip result-abort*
                  ;; TRIAL events
                  trial-event trial-start
                  ;; misc complex types
                  (and unexpected (not error*))
                  (or unexpected-failure error*)
                  (or unexpected-failure abort*)
                  (or error* trial-start)))
    (let* ((implicit-output
             (make-array 0 :element-type 'character :fill-pointer 0
                         :adjustable t))
           (output0 (make-array 0 :element-type 'character :fill-pointer 0
                                :adjustable t))
           (output1 (make-array 0 :element-type 'character :fill-pointer 0
                                :adjustable t))
           (output2 (make-array 0 :element-type 'character :fill-pointer 0
                                :adjustable t))
           ;; Getting a backtrace is very slow on most Lisps.
           (*gather-backtrace* nil)
           (describe-type '(and unexpected (not unhandled-error)))
           (trial0 (with-output-to-string (stream output0)
                     (let ((*print-duration* timing)
                           (try::*testing-timing* t))
                       (try '%collect-test-case
                            :print type
                            :describe describe-type
                            :collect type
                            :stream stream))))
           (trial1 (with-output-to-string (stream output1)
                     (let ((*print-duration* timing)
                           (try::*testing-timing* t))
                       (replay-events trial0
                                      :print type
                                      :describe describe-type
                                      :collect type
                                      :stream stream)))))
      (with-output-to-string (stream output2)
        (let ((*print-duration* timing)
              (try::*testing-timing* t))
          (replay-events trial1
                         :print type
                         :describe describe-type
                         :stream stream)))
      (unless timing
        (with-output-to-string (stream implicit-output)
          (with-silent-implicit-try
            (let ((*print-duration* timing)
                  (try::*testing-timing* t)
                  (*print* type)
                  (*describe* describe-type)
                  (*collect* type)
                  (*stream* stream))
              (%collect-test-case))))
        (is (null (mismatch% implicit-output output0 :max-suffix-length 40))
            :msg (list "TRY and TRY/IMPLICIT output are the same for type ~S."
                       type)))
      (with-expected-outcome ((if (alexandria:featurep :clisp)
                                  '(or failure success)
                                  'success))
        (is (null (mismatch% output0 output1 :max-suffix-length 40))
            :msg (list "Printing collected output is 1-stable for type ~S."
                       type)))
      (is (null (mismatch% output1 output2 :max-suffix-length 40))
          :msg (list "Printing collected output is 2-stable for type ~S."
                     type)))))

(deftest test-try/collect-and-replay/timing ()
  (test-try/collect-and-replay :timing t))

(deftest test-try/debug-outcome-type ()
  ;; This used to fail by invoking the debugger not only on MY-ERR but
  ;; also on the resulting UNHANDLED-ERRORs.
  (let ((event-counts
          (list-total-outcomes
           (try (lambda ()
                  (with-test (t0)
                    (with-test (t1)
                      (invokes-debugger
                          (unhandled-error :handler #'abort-trial)
                        (error 'my-err)))))
                :print nil
                :describe nil
                :debug 'unhandled-error))))
    (is (equal event-counts (to-counts '((abort* 1)
                                         (expected-success 1)))))))

(deftest test-try/debug-first-restart-continues ()
  (try::with-debugger-hook #'invoke-first-restart
    (check-try-output ('%simple-nested :describe nil :debug 'event)
                      "%SIMPLE-NESTED
  T0
    ✓ (IS T)
    × (IS NIL)
  × T0 ×1 ✓1
  T1
    ! \"my-msg\" (MY-ERR)
  ! T1 !1
  T2
    ! non-local exit
  ! T2 !1
× %SIMPLE-NESTED !2 ×1 ✓1
")))

(deftest %simple-nested ()
  (with-test (t0)
    (is t)
    (is nil))
  (with-test (t1)
    (error 'my-err))
  (catch 'foo
    (with-test (t2)
      (throw 'foo nil))))

(defun invoke-first-restart (condition)
  (let ((restart (first (compute-restarts condition))))
    (assert (member (restart-name restart) '(record-event abort-trial)))
    (if (eq (restart-name restart) 'record-event)
        (invoke-restart restart)
        (invoke-restart restart condition))))

(deftest test-try/unwind-from-verdict ()
  (try (lambda ()
         (with-test (t0)
           ;; Any non-local-exit other than RECORD-EVENT used to
           ;; forget to record the verdict, which left a TRIAL-START
           ;; unpaired and the printer state inconsistent.
           (handler-bind ((unexpected-verdict-failure #'skip-trial))
             (with-test (t1)
               (is nil)))))
       :print nil))


(deftest test-try/printer-variables ()
  (dolist (pretty '(nil t))
    (dolist (escape '(nil t))
      (let ((*print-pretty* pretty)
            (*print-escape* escape))
        (check-try-output ('%all-check-outcome-types
                           ;; Exclude UNHANDLED-ERROR because its
                           ;; backtrace is hard to check.
                           :describe '(not unhandled-error))
                          "%ALL-CHECK-OUTCOME-TYPES
  ✓ (IS (EQL #1=(1+ 3) 4))
    where
      #1# = 4
  × (IS (= 5 6))
    debug info
  ✓× (IS (EQUAL 3 #1=(1+ 2)))
     where
       #1# = 3
  ×× (IS NIL)
     more info
  - (IS T)
  ! \"my-msg\" (MY-ERR)
! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
")))))


(deftest %random-crap ()
  (format *debug-io* "Hello, World!"))

(deftest test-try/random-crap-on-output-stream ()
  (check-try-output ('%random-crap :use-debug-io t
                     :expected-outcome 'success)
                    "Hello, World!
✓ %RANDOM-CRAP
")
  (check-try-output ('%random-crap
                     :print '(or outcome trial-start)
                     :use-debug-io t
                     :expected-outcome 'success)
                    "%RANDOM-CRAPHello, World!
✓ %RANDOM-CRAP
"))

(deftest test-try/nested ()
  (is (passedp (with-failure-expected ()
                 (try (lambda () (is nil)) :print nil))))
  (is (passedp (with-skip ()
                 (try (lambda () (is nil)) :print nil)))))

(defun test-try/compact-printing ()
  (with-std-try
    (let ((*gather-backtrace* nil)
          (*print-compactly* t))
      (check-try-output ('%collect-test-case)
                        "%COLLECT-TEST-CASE
  %ALL-CHECK-OUTCOME-TYPES ✓
    × (IS (= 5 6))
      debug info
    ✓× (IS (EQUAL 3 #1=(1+ 2)))
       where
         #1# = 3
    ××-
    ! \"my-msg\" (MY-ERR)
  ! %ALL-CHECK-OUTCOME-TYPES !1 ×1 ✓×1 -1 ××1 ✓1
  FAILING-OUTER
    PASSING-INNER ✓✓ => ✓
    × (IS NIL)
  × FAILING-OUTER ×1 ✓2
  ERRORING-OUTER
    PASSING-INNER ✓✓ => ✓
    ! \"my-msg\" (MY-ERR)
  ! ERRORING-OUTER !1 ✓2
× %COLLECT-TEST-CASE !2 ×2 ✓×1 -1 ××1 ✓5
"))))

(deftest test-try/compact-and-deferred-printing ()
  (with-std-try
    (let ((*gather-backtrace* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*print-parent* nil)
          (*categories* (ascii-std-categories)))
      (check-try-output ('%collect-test-case
                         :print 'leaf)
                        ".F:f-!..F..!

;; UNEXPECTED-RESULT-FAILURE (F) in %COLLECT-TEST-CASE %ALL-CHECK-OUTCOME-TYPES:
(IS (= 5 6))
debug info

;; UNEXPECTED-RESULT-SUCCESS (:) in %COLLECT-TEST-CASE %ALL-CHECK-OUTCOME-TYPES:
(IS (EQUAL 3 #1=(1+ 2)))
where
  #1# = 3

;; UNHANDLED-ERROR (!) in %COLLECT-TEST-CASE %ALL-CHECK-OUTCOME-TYPES:
\"my-msg\" (MY-ERR)

;; UNEXPECTED-RESULT-FAILURE (F) in %COLLECT-TEST-CASE FAILING-OUTER:
(IS NIL)

;; UNHANDLED-ERROR (!) in %COLLECT-TEST-CASE ERRORING-OUTER:
\"my-msg\" (MY-ERR)
")
      (check-try-output ('%collect-test-case
                         :print 'leaf
                         :describe '(or unexpected-failure error*))
                        ".F:f-!..F..!

;; UNEXPECTED-RESULT-FAILURE (F) in %COLLECT-TEST-CASE %ALL-CHECK-OUTCOME-TYPES:
(IS (= 5 6))
debug info

;; UNHANDLED-ERROR (!) in %COLLECT-TEST-CASE %ALL-CHECK-OUTCOME-TYPES:
\"my-msg\" (MY-ERR)

;; UNEXPECTED-RESULT-FAILURE (F) in %COLLECT-TEST-CASE FAILING-OUTER:
(IS NIL)

;; UNHANDLED-ERROR (!) in %COLLECT-TEST-CASE ERRORING-OUTER:
\"my-msg\" (MY-ERR)
"))))

(defun test-try/implicit ()
  (is (passedp (with-silent-implicit-try (with-test (t0)))))
  (is (null *try-debug*))
  (is (passedp (try (lambda ()
                      (is (null *try-debug*)))
                    :print nil)))
  ;; An explicit call to TRY shall not inherit settings for
  ;; TRY/IMPLICIT.
  (is (passedp (with-silent-implicit-try
                 (let ((*collect* t))
                   (with-test (t0)
                     (is (passedp (try (lambda ()
                                         (is (not (eq *try-collect*
                                                      t))))
                                       :print nil)))))))))
