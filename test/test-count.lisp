(in-package :try-test)

(deftest test-try/count ()
  (test-count/all)
  (test-count/replay)
  (test-count/replay/categories-and-count))


(deftest test-count/all ()
  (let ((*count* t))
    (check-try-output ((named-lambda-test t0 ()
                         (with-test (t1)
                           (is t))))
                      "T0
  T1
    ✓ (IS T)
  ✓ T1 ✓1
✓ T0 ✓2
")))

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
