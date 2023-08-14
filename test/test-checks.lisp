(in-package :try-test)

(deftest test-checks ()
  (test-signals)
  (test-signals-not)
  (test-invokes-debugger)
  (test-invokes-debugger-not)
  (test-fails)
  (test-in-time)
  (test-match-values)
  (test-same-set-p))


(deftest test-signals ()
  (test-signals/success)
  (test-signals/success-outcomes)
  (test-signals/failure)
  (test-signals/failure/type)
  (test-signals/failure/pred)
  (test-signals/success/msg)
  (test-signals/failure/debug)
  (test-signals/rewrite)
  (test-signals/restarts))

(define-condition cond-a ()
  ())

(define-condition cond-b ()
  ())

(define-condition my-err (error)
  ((msg :initform "my-msg" :initarg :msg))
  (:report (lambda (c stream)
             (format stream "~A" (slot-value c 'msg)))))

(define-condition my-cond ()
  ((msg :initform "my-msg" :initarg :msg))
  (:report (lambda (c stream)
             (format stream "~A" (slot-value c 'msg)))))

(deftest test-signals/success ()
  (is (eql (signals (cond-a :handler nil)
             (signal 'cond-a)
             47)
           47))
  (is (null (signals (my-err :handler t)
              (error 'my-err)
              47)))
  (is (eql (signals (my-err :handler #'continue)
             (cerror "continue" (make-condition 'my-err))
             47)
           47))
  (is (eql (catch 'foo
             (signals (my-err :on-nlx nil)
               (throw 'foo 47)))
           47)
      :msg ":ON-NLX NIL and nlx")
  (signals ((and unexpected failure) :msg ":ON-NLX NIL and return")
    (signals (my-err :on-nlx nil)
      47))
  (is (eql
       (signals (my-err :on-return nil)
         47)
       47)
      :msg ":ON-RETURN NIL and return")
  (signals ((and unexpected failure) :msg ":ON-RETURN NIL and nlx")
    (catch 'foo
      (signals (my-err :on-return nil)
        (throw 'foo 47)))))

(deftest test-signals/success-outcomes ()
  (signals ((and expected success) :pred "matches" :handler nil)
    (signals ((and expected success) :pred "condition of type" :handler nil)
      (signals (cond-a :handler nil)
        (signal 'cond-a)
        47))))

(deftest test-signals/failure ()
  (signals ((and unexpected failure))
    (signals (cond-a :handler nil)
      (signal 'cond-b))))

(deftest test-signals/failure/type ()
  (signals ((and unexpected failure) :pred "condition of type")
    (signals (cond-a :handler nil)
      (signal 'cond-b))))

(deftest test-signals/failure/pred ()
  (handler-case
      (signals ((and unexpected failure)
                 :pred "predicate did not match")
        (signals (error :pred (constantly nil))
          (error 'my-err)))
    (my-err ())))

(deftest test-signals/success/msg ()
  (signals ((and expected success) :pred "hello there" :handler nil)
    (signals (error :msg ("hello ~A" "there"))
      (error "xxx"))))

(deftest test-signals/failure/debug ()
  (signals ((and unexpected failure) :pred "hello there")
    (signals (error :ctx ("hello ~A" "there"))
      47)))

(deftest test-signals/rewrite ()
  (signals ((and unexpected failure) :pred "No condition of type"
             :name 'signal-rewrite-1)
    (signals ((and unexpected failure)
              :pred "This signals a condition of type MY-ERR"
              :name 'signals-rewrite :handler nil)
      (signals (my-err :pred "my-msg"
                 :msg ("This signals a condition of type MY-ERR ~
                        that matches \"my-msg\".")
                 :ctx (cond (*condition-matched-p*
                             ())
                            (*best-matching-condition*
                             (list "The predicate did not match ~S."
                                   (try::%describe-condition-for-matching
                                    *best-matching-condition*)))
                            (t
                             (list "No condition of type ~S was handled."
                                   'my-err))))
        47))))


(deftest test-signals/restarts ()
  (test-signals/success/force-failure)
  (test-signals/failure/force-success)
  (test-signals/failure/retry))

(deftest test-signals/success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (signals (cond-a :handler nil)
                                 (signal 'cond-a)
                                 47)))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest test-signals/failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (signals (cond-a :handler nil)
                                 47)))))
                  '((expected-success 2))))


(deftest test-signals/failure/retry ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (signals ((and expected success) :handler nil)
                               (signals-not ((and unexpected failure))
                                 (let ((x 0))
                                   (handler-bind (((and unexpected failure)
                                                    #'retry-check))
                                     (signals (cond-a :handler nil)
                                       (incf x)
                                       (when (= x 2)
                                         (signal 'cond-a)
                                         47)))))))))
                  '((expected-success 4))))


(deftest test-signals-not ()
  (test-signals/not-success)
  (test-signals/not-success/handled-error)
  (test-signals/not-failure)
  (test-signals-not-restarts))

(deftest test-signals/not-success ()
  (signals-not (error)
    47))

(deftest test-signals/not-success/handled-error ()
  (signals-not (error)
    (ignore-errors
     (error "xxx"))))

(deftest test-signals/not-failure ()
  (signals ((and unexpected failure))
    (signals-not (condition)
      (signal 'cond-a))))


(deftest test-signals-not-restarts ()
  (test-signals/not-success/force-failure)
  (test-signals/not-failure/force-success)
  (test-signals/not-failure/retry))

(deftest test-signals/not-success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (signals-not (cond-a)
                                 47)))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest test-signals/not-failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (signals-not (cond-a :handler nil)
                                 (signal 'cond-a)
                                 47)))))
                  '((expected-success 2))))

(deftest test-signals/not-failure/retry ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (signals ((and expected success) :handler nil)
                               (signals-not ((and unexpected failure))
                                 (let ((x 0))
                                   (handler-bind (((and unexpected failure)
                                                    #'retry-check))
                                     (signals-not (cond-a)
                                       (incf x)
                                       (if (= x 1)
                                           (signal 'cond-a)
                                           47)))))))))
                  '((expected-success 4))))


(deftest test-invokes-debugger ()
  (is (null (invokes-debugger (cond-a :handler t)
              (invoke-debugger (make-condition 'cond-a))
              47)))
  (is (eql (invokes-debugger (my-err :handler #'continue)
             (with-simple-restart (continue "continue")
               (invoke-debugger (make-condition 'my-err)))
             47)
           47))
  (is (eql (catch 'foo
             (invokes-debugger (my-err :on-nlx nil)
               (throw 'foo 47)))
           47)
      :msg ":ON-NLX NIL and nlx")
  (signals ((and unexpected failure) :pred "No condition of type")
    (invokes-debugger (my-err))
    :msg "No condition of type failure.")
  (signals ((and unexpected failure) :pred "The predicate did not match")
    ;; This outer one is to prevent entering the debugger on the
    ;; failure of the inner INVOKES-DEBUGGER.
    (invokes-debugger (t)
      (invokes-debugger (my-err :pred "xxx")
        (invoke-debugger (make-condition 'my-err :msg "hhh"))))
    :msg "The predicate did not match failure.")
  (signals ((and unexpected failure) :msg ":ON-NLX NIL and return")
    (invokes-debugger (my-err :on-nlx nil)
      47))
  (is (eql
       (invokes-debugger (my-err :on-return nil)
         47)
       47)
      :msg ":ON-RETURN NIL and return")
  (signals ((and unexpected failure) :msg ":ON-RETURN NIL and nlx")
    (catch 'foo
      (invokes-debugger (my-err :on-return nil)
        (throw 'foo 47))))
  (test-invokes-debugger/restarts))


(deftest test-invokes-debugger/restarts ()
  (invokes-debugger-success/force-failure)
  (invokes-debugger-failure/force-success)
  (invokes-debugger-failure/retry))

(deftest invokes-debugger-success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (invokes-debugger (cond-a)
                                 (error 'cond-a))
                               47))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest invokes-debugger-failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (invokes-debugger (cond-a)
                                 47)))))
                  '((expected-success 2))))

(deftest invokes-debugger-failure/retry ()
  (check-outcomes (lambda ()
                    (signals ((and expected success) :handler nil)
                      (signals-not ((and unexpected failure))
                        (let ((x 0))
                          (handler-bind (((and unexpected failure)
                                           #'retry-check))
                            (invokes-debugger (cond-a)
                              (incf x)
                              (when (= x 2)
                                (invoke-debugger
                                 (make-condition 'cond-a)))))
                          (is (= x 2))))))
                  '((expected-success 4))))


(deftest test-invokes-debugger-not ()
  (is (eql (invokes-debugger-not (cond-a :handler t)
             47)
           47))
  (signals ((and unexpected failure))
    (invokes-debugger-not (cond-a :handler t)
      (invoke-debugger (make-condition 'cond-a))))
  (invokes-debugger (cond-a)
    (signals ((and unexpected failure))
      (invokes-debugger-not (cond-a :handler nil)
        (invoke-debugger (make-condition 'cond-a)))))
  (let ((continued nil))
    (signals ((and unexpected failure))
      (invokes-debugger-not (my-err :handler #'continue)
        (with-simple-restart (continue "continue")
          (invoke-debugger (make-condition 'my-err)))
        (setq continued t)))
    (is continued))
  (signals ((and expected success) :pred "No condition of type"
                             :msg "No condition of type success.")
    (invokes-debugger-not (my-err)))
  (signals ((and expected success) :pred "The predicate did not match")
    ;; This is to prevent entering the debugger on the failure of the
    ;; INVOKES-DEBUGGER-NOT.
    (invokes-debugger (t)
      (invokes-debugger-not (my-err :pred "xxx")
        (invoke-debugger (make-condition 'my-err :msg "hhh"))))
    :msg "The predicate did not match failure.")
  (is (eql (catch 'foo
             (invokes-debugger-not (my-err :on-nlx nil)
               (throw 'foo 47)))
           47)
      :msg ":ON-NLX NIL and nlx")
  (signals ((and expected success) :msg ":ON-NLX NIL and return")
    (invokes-debugger-not (my-err :on-nlx nil)
      47))
  (is (eql (signals-not (outcome)
             (invokes-debugger-not (my-err :on-return nil)
               47))
           47)
      :msg ":ON-RETURN NIL and return")
  (signals ((and expected success) :msg ":ON-RETURN NIL and nlx")
    (catch 'foo
      (invokes-debugger-not (my-err :on-return nil)
        (throw 'foo 47))))
  (test-invokes-debugger-not-restarts))


(deftest test-invokes-debugger-not-restarts ()
  (invokes-debugger-not-success/force-failure)
  (invokes-debugger-not-failure/force-success)
  (invokes-debugger-not-failure/retry))

(deftest invokes-debugger-not-success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (invokes-debugger-not (cond-a))
                               47))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest invokes-debugger-not-failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (invokes-debugger-not (cond-a)
                                 (invoke-debugger (make-condition 'cond-a)))
                               47))))
                  '((expected-success 2))))

(deftest invokes-debugger-not-failure/retry ()
  (check-outcomes (lambda ()
                    (signals ((and expected success) :handler nil)
                      (signals-not ((and unexpected failure))
                        (let ((x 0))
                          (handler-bind (((and unexpected failure)
                                           #'retry-check))
                            (invokes-debugger-not (cond-a)
                              (incf x)
                              (when (= x 1)
                                (invoke-debugger
                                 (make-condition 'cond-a)))))
                          (is (= x 2))))))
                  '((expected-success 4))))


(deftest test-fails ()
  (test-fails/success/error)
  (test-fails/success/throw)
  (test-fails/failure)
  (test-fails/restarts))

(deftest test-fails/success/error ()
  (signals (error :pred "xxx")
    (fails ()
      (error "xxx"))))

(deftest test-fails/success/throw ()
  (catch 'xxx
    (fails ()
      (throw 'xxx nil))))

(deftest test-fails/failure ()
  (signals ((and unexpected failure))
    (fails ())))


(deftest test-fails/restarts ()
  (test-fails/success/force-failure)
  (test-fails/failure/force-success)
  (test-fails/failure/retry))

(deftest test-fails/success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (catch 'xxx
                                 (fails ()
                                   (throw 'xxx 47)))))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest test-fails/failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (fails ()
                                 47)))))
                  '((expected-success 2))))

(deftest test-fails/failure/retry ()
  (check-outcomes (lambda ()
                    (is (let ((x 0))
                          (signals ((and expected success) :handler nil)
                            (signals-not ((and unexpected failure))
                              (handler-bind (((and unexpected failure)
                                               #'retry-check))
                                (catch 'xxx
                                  (fails ()
                                    (incf x)
                                    (when (= x 2)
                                      (throw 'xxx 47))))))))))
                  '((expected-success 4))))


(deftest test-in-time ()
  (in-time/success)
  (in-time/failure)
  (test-in-time/restarts))

(defun %in-time/success ()
  (in-time (1)
    47))

(defun %in-time/failure ()
  (let ((try::*testing-timing* 0.01))
    (in-time (0.001)
      47)))

(deftest in-time/success ()
  (check-outcomes '%in-time/success '((expected-success 1))))

(deftest in-time/failure ()
  (check-outcomes '%in-time/failure '((unexpected-failure 1))))


(deftest test-in-time/restarts ()
  (in-time-success/force-failure)
  (in-time-failure/force-success)
  (in-time-failure/retry))

(deftest in-time-success/force-failure ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and expected success)
                                              #'force-unexpected-failure))
                               (%in-time/success)))))
                  '((expected-success 1)
                    (unexpected-failure 1))))

(deftest in-time-failure/force-success ()
  (check-outcomes (lambda ()
                    (is (eql 47
                             (handler-bind (((and unexpected failure)
                                              #'force-expected-success))
                               (%in-time/failure)))))
                  '((expected-success 2))))

(deftest in-time-failure/retry ()
  (check-outcomes (lambda ()
                    (signals ((and expected success) :handler nil)
                      (signals-not ((and unexpected failure))
                        (let ((x 0.01))
                          (handler-bind (((and unexpected failure)
                                           #'retry-check))
                            (let ((try::*testing-timing* 0.2))
                              (in-time ((prog1 x (incf x)))
                                47)))))))
                  '((expected-success 3))))


(deftest test-match-values ()
  (is (match-values (values 1 2)
        (= * 1)
        (= * 2)))
  (is (not (match-values (values 1 2 3)
             (= * 1)
             (= * 2)))))



(deftest test-same-set-p ()
  (is (same-set-p '() '()))
  (is (same-set-p '("x") '("x") :test #'equal))
  (is-ctx-captures '((try::only-in-1 (1) nil t)
                     (try::only-in-2 (2) nil t))
    (is (not (same-set-p '(1) '(2))))))
