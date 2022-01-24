(in-package :try)

(defsection @try/comparing-floats (:title "Comparing Floats")
  "Float comparisons following
   [https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/](https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/)."
  (float-~= function)
  (*max-diff-in-value* variable)
  (*max-diff-in-ulp* variable)
  (float-~< function)
  (float-~> function))

(defvar *max-diff-in-value* 1e-16
  "The default value of the MAX-DIFF-IN-VALUE argument of FLOAT-~=.")

(defvar *max-diff-in-ulp* 2
  "The default value of the MAX-DIFF-IN-ULP argument of FLOAT-~=.")

(defun float-~= (x y &key
                 (max-diff-in-value *max-diff-in-value*)
                 (max-diff-in-ulp *max-diff-in-ulp*))
  "Return whether two numbers, X and Y, are approximately equal either
  according to MAX-DIFF-IN-VALUE or MAX-DIFF-IN-ULP.

  If the absolute value of the difference of two floats is not greater
  than MAX-DIFF-IN-VALUE, then they are considered equal.

  If two floats are of the same sign and the number of representable
  floats (ULP, unit in the last place) between them is less than
  MAX-DIFF-IN-ULP, then they are considered equal.

  If neither X nor Y are floats, then the comparison is done with `=`.
  If one of them is a DOUBLE-FLOAT, then the other is converted to a
  double float, and the comparison takes place in double float space.
  Else, both are converted to SINGLE-FLOAT and the comparison takes
  place in single float space."
  (cond ((and (not (typep x 'float))
              (not (typep y 'float)))
         (= x y))
        ((or (typep x 'double-float)
             (typep y 'double-float))
         (double-float-~= (float x 0d0) (float y 0d0)
                          (float max-diff-in-value 0d0)
                          max-diff-in-ulp))
        (t
         (single-float-~= (float x 0.0) (float y 0.0)
                          (float max-diff-in-value 0.0)
                          max-diff-in-ulp))))

(defun single-float-~= (x y max-diff-in-value max-diff-in-ulp)
  (declare (type single-float x y))
  (or (<= (abs (- x y)) max-diff-in-value)
      (and (= (float-sign x) (float-sign y))
           (let ((x-ulp (ieee-floats:encode-float32 x))
                 (y-ulp (ieee-floats:encode-float32 y)))
             (<= (abs (- x-ulp y-ulp)) max-diff-in-ulp)))))

(defun double-float-~= (x y max-diff-in-value max-diff-in-ulp)
  (declare (type double-float x y))
  (or (<= (abs (- x y)) max-diff-in-value)
      (and (= (float-sign x) (float-sign y))
           (let ((x-ulp (ieee-floats:encode-float64 x))
                 (y-ulp (ieee-floats:encode-float64 y)))
             (<= (abs (- x-ulp y-ulp)) max-diff-in-ulp)))))


(defun float-~< (x y &key
                 (max-diff-in-value *max-diff-in-value*)
                 (max-diff-in-ulp *max-diff-in-ulp*))
  "Return whether X is approximately less than Y. Equivalent to `<`,
  but it also allows for approximate equality according to FLOAT-~=."
  (or (float-~= x y
                :max-diff-in-value max-diff-in-value
                :max-diff-in-ulp max-diff-in-ulp)
      (< x y)))

(defun float-~> (x y &key
                 (max-diff-in-value *max-diff-in-value*)
                 (max-diff-in-ulp *max-diff-in-ulp*))
  "Return whether X is approximately greater than Y. Equivalent to `>`,
  but it also allows for approximate equality according to FLOAT-~=."
  (or (float-~= x y
                :max-diff-in-value max-diff-in-value
                :max-diff-in-ulp max-diff-in-ulp)
      (> x y)))
