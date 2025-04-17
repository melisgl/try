(in-package :try-test)

(deftest test-is ()
  (test-is/simple)
  (test-is/custom-message)
  (test-is/print-newline)
  (test-is/failure-debug-info)
  (test-is/rewrite-default-behavior)
  (test-is/match-values)
  (test-is/capture)
  (test-is/restarts))

(deftest test-is/simple ()
  (is t)
  (signals ((and unexpected failure))
    (is nil))
  (is (eq 'x 'x))
  (signals ((and unexpected failure))
    (is (eq 'x 'y))))

(deftest test-is/custom-message ()
  (signals ((and expected success) :pred "custom message" :handler nil)
    (is t :msg ("custom ~A" "message")))
  (signals ((and expected success) :pred "custom message" :handler nil)
    (let ((msg "custom message"))
      (is t :msg msg))))

(deftest test-is/print-newline ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (is (null (mismatch% (handler-case
                             (is t :msg "FORMAT-CONTROL~%with new line.")
                           (outcome (c)
                             (with-output-to-string (s)
                               (try::write-event c s :ctx t :terse nil))))
                         "EXPECTED-SUCCESS in check:
  FORMAT-CONTROL
  with new line."))))
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro :clisp)))
    (is (null (mismatch% (handler-case
                             (is t :msg (list "FORMAT-ARGS~A with new line."
                                              (format nil "~%")))
                           (outcome (c)
                             (with-output-to-string (s)
                               (try::write-event c s :ctx t :terse nil))))
                         "EXPECTED-SUCCESS in check:
  FORMAT-ARGS

  with new line.")))))

(deftest test-is/failure-debug-info ()
  (signals ((and unexpected failure) :pred "debug info")
    (is nil :ctx ("debug ~A" "info"))))

(deftest test-is/rewrite-default-behavior ()
  (signals ((and unexpected failure) :pred "= 5" :name 'is-rewrite)
    (is (not (equal (1- 6) (1+ 4)))
        :msg ("~S" *is-form*)
        :ctx ("my where~:@_~{  ~S = ~S~^~:@_~}"
              ;; This is not quite right. *IS-CAPTURES* holds
              ;; CAPTUREs.
              (apply #'append *is-captures*))
        :print-captures nil)))

(deftest test-is/match-values ()
  ;; no values
  (is (match-values (values)))
  ;; single value success
  (is (match-values 1
        (= * 1)))
  ;; success with different types
  (is (match-values (values 1 "sdf")
        (= * 1)
        (string= * "sdf")))
  ;; too few values
  (is (not (match-values 1
             (= * 1)
             (string= * "sdf"))))
  ;; too many values
  (is (not (match-values (values 1 "sdf" 3)
             (= * 1)
             (string= * "sdf"))))
  ;; too many values, but truncated
  (is (match-values (values 1 "sdf" 3)
        (:truncate t)
        (= * 1)
        (string= * "sdf")))
  (is (match-values 7
        (:on-length-mismatch nil)
        (eql * 7)
        (null *)))
  (is-ctx-captures '(((values 1 7) (1 7) t t))
    (is (match-values (values 1 7)
          (= * 2)
          (= * 7)))))


(deftest test-is/capture ()
  (test-is/capture/implicit-and-explicit)
  (test-is/capture/constantishp)
  (test-is/capture/no-implicit)
  (test-is/capture/evaluation-order)
  (test-is/capture/duplicate)
  (test-is/capture/nested-subs)
  (test-is/capture/improper-list-literal))

(deftest test-is/capture/implicit-and-explicit ()
  (is-ctx-captures '(((null t) nil nil t))
    (is (and (% (null t)))))
  (is-ctx-captures '((7 7 nil t)
                     ((and (null (% 7))) nil nil nil))
    (is (null (and (null (% 7))))))
  (is-ctx-captures '(((null t) nil nil t))
    (is (identity (identity (% (null t))))
        :capture nil))
  (is-ctx-captures '(((values 1 2) (1 2) t t))
    (is (and (null (%% (values 1 2))))))
  ;; explicit nesting
  (is-ctx-captures '((3 3 nil t)
                     ((1+ (% 3)) 4 nil t))
    (is (and (null (% (1+ (% 3)))))))
  ;; explicit nesting in implicit capture
  (is-ctx-captures '((3 3 nil t)
                     ((1+ (% 3)) 4 nil nil))
    (is (identity (1+ (% 3))))))

(deftest test-is/capture/constantishp ()
  (is-ctx-captures ()
    (is (identity nil)))
  (is-ctx-captures ()
    (is (identity :key)))
  (is-ctx-captures ()
    (is (identity 5)))
  (is-ctx-captures ()
    (is (identity #\a)))
  (is-ctx-captures ()
    (is (identity "abc")))
  (is-ctx-captures ()
    (is (identity #(a b c))))
  (is-ctx-captures ()
    (is (identity #2A((0 0) (0 0)))))
  (is-ctx-captures ()
    (is (identity #P"/tmp/")))
  (is-ctx-captures ()
    (is (identity #'1+))))

(deftest test-is/capture/no-implicit ()
  (is-ctx-captures ()
    (is (= 5 (1+ 5)) :capture nil)))

(deftest test-is/capture/evaluation-order ()
  (let ((x 0))
    (is (equal (incf x) (progn
                          (is (= x 1))
                          1))))
  (let ((x 0))
    (is (identity (equal (% (incf x))
                         (% (progn
                              (is (= x 1))
                              1))))))
  ;; Evaluation rules is macros generally unknown, so we don't do
  ;; automatic captures.
  (let ((x nil))
    (is (not (and x (is nil :msg "Must not be evaluated."))))))

(deftest test-is/capture/duplicate ()
  (with-failure-expected ((alexandria:featurep :abcl))
    (with-test ("same variable captured implicitly only once")
      (is-ctx-captures '((x 1 nil nil))
        (let ((x 1))
          (is (< x x)))))
    (with-test ("same variable captured twice explicitly")
      (is-ctx-captures '((x 1 nil t)
                         (x 1 nil t))
        (let ((x 1))
          (is (< (% x) (% x))))))
    (with-test ("explicit and implicit capture deduplicated")
      (is-ctx-captures '(((1+ 2) 3 nil t))
        (is (equal (% (1+ 2)) 5))))
    (with-test ("explicit %% dominates implicit capture")
      (is-ctx-captures '(((values (1+ 2) t) (3 t) t t))
        (is (equal (%% (values (1+ 2) t)) 5))))
    ;; ABCL and ECL has some EQness related issue.
    ;; https://github.com/armedbear/abcl/issues/606
    ;; https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665
    (with-test ("explicit CAPTURE-VALUES dominates implicit capture")
      (with-failure-expected ((alexandria:featurep '(:or :abcl :ecl :cmucl)))
        (is-ctx-captures '(((values (1+ 2) t) (3 t) t t))
          (is (equal (capture-values (values (1+ 2) t)) 5)))))
    (with-test ("implicit and explicit deduplicated, plus nesting")
      (is-ctx-captures '(((+ 2 5) 7 nil t)
                         ((1+ (% (+ 2 5))) 8 nil t))
        (is (not (% (1+ (% (+ 2 5))))))))))

(deftest test-is/capture/nested-subs ()
  (is-ctx-captures '(((1+ 5) 6 nil nil)
                     ((identity (1+ 5)) 6 nil nil))
    (is (null (identity (1+ 5)))))
  (is-ctx-captures '(((1+ 5) 6 nil nil)
                     ((identity (1+ 5)) 6 nil nil))
    (is (null (identity (1+ 5)))))
  (is-ctx-captures '(((1+ 5) 6 nil nil)
                     ((list (1+ 5)) (6) nil nil))
    (is (endp (list (1+ 5))))))

(deftest test-is/capture/improper-list-literal ()
  (check-try-output ((named-lambda-test foo77 ()
                       (is (equalp 1 '(1 . 2)))))
                    "FOO77
  × (IS (EQUALP 1 '(1 . 2)))
× FOO77 ×1
"))


(deftest test-is/restarts ()
  (test-is/success/skip)
  (test-is/success/skip/with-skip)
  (test-is/success/force-failure)
  (test-is/failure/skip)
  (test-is/failure/retry)
  (test-is/failure/force-success))

(deftest test-is/success/skip ()
  (check-outcomes (lambda ()
                    (is (not (handler-bind (((and expected success)
                                              #'skip-check))
                               (is t)))))
                  '((skip 1)
                    (expected-success 1))))

(deftest test-is/success/skip/with-skip ()
  (check-outcomes (lambda ()
                    (with-skip ()
                      (is t)))
                  '((skip 1))))

(deftest test-is/success/force-failure ()
  (check-outcomes (lambda ()
                    (is (not
                         (handler-bind (((and expected success)
                                          #'force-unexpected-failure))
                           (is t)))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest test-is/failure/skip ()
  (check-outcomes (lambda ()
                    (with-skip ()
                      (is nil)))
                  '((skip 1))))

(deftest test-is/failure/retry ()
  (check-outcomes (lambda ()
                    (is (let ((x 0))
                          (signals ((and expected success) :handler nil)
                            (signals-not ((and unexpected failure))
                              (handler-bind (((and unexpected failure)
                                               #'retry-check))
                                (is (progn (incf x)
                                           (= x 2)))))))))
                  '((expected-success 4))))

(deftest test-is/failure/force-success ()
  (check-outcomes (lambda ()
                    (is (handler-bind (((and unexpected failure)
                                         #'force-expected-success))
                          (is nil))))
                  '((expected-success 2))))
