(in-package :try-test)

(deftest test-floats ()
  (test-float-~=))

(deftest test-float-~= ()
  (is (float-~= 0 0))
  (is (float-~= 0.0 -0.0))
  (with-test (positive)
    (loop repeat 1000 do
      (let* ((x (random most-positive-single-float))
             (y (ieee-floats:decode-float32
                 (1+ (ieee-floats:encode-float32 x)))))
        ;; Silence the successes in this high-volume test.
        (unless (float-~= x y)
          (is (float-~= x y))
          (return)))))
  (with-test (negative)
    (loop repeat 1000 do
      (let* ((x (- (random most-positive-single-float)))
             (y (ieee-floats:decode-float32
                 (1+ (ieee-floats:encode-float32 x)))))
        (unless (float-~= x y)
          (is (float-~= x y))
          (return))))))
