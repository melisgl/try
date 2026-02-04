(in-package :try-test)

(deftest test-try/count ()
  (let ((*collect* 'unexpected))
    (test-count/all)
    (test-count/replay)
    (test-count/replay/categories-and-count)
    (test-count/trial-start)))


(deftest test-count/all ()
  (check-try-output ((named-lambda-test t0 ()
                       (with-test (t1)
                         (is t)))
                     :count t)
                    "T0
  T1
    ✓ (IS T)
  ✓ T1 ✓1
✓ T0 ✓2
"))

(deftest test-count/replay ()
  (check-try-output ((with-silent-implicit-try
                       (with-test (t0)
                         (with-test (t1)
                           (is t))))
                     :replay-events t)
                    "⋅ T0 ⋅1
"))

(deftest test-count/replay/categories-and-count ()
  (let ((trial (with-silent-implicit-try
                 (with-test (t0)
                   (with-test (t1)
                     (is t))))))
    (let ((*categories* 'invalid)
          (*count* 'invalid))
      (check-try-output (trial :replay-events t)
                        "⋅ T0 ⋅1
"))))

(deftest test-count/trial-start ()
  (let ((trial (check-try-output
                   ((named-lambda-test t0 ()
                      (with-test (t1)
                        (with-test (t2)
                          (is t))))
                    :categories '((trial-start :marker "s"))
                    :count t)
                   "s T0
  s T1
    s T2
      (IS T)
    T2 s1
  T1 s2
T0 s3
")))
    (check-try-output (trial :replay-events t)
                      "T0 s3
")))
