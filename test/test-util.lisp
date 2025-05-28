(in-package :try-test)

(deftest test-utils ()
  (test-on-values)
  (test-with-retry)
  (test-lambda-list-to-arglist-form))

(deftest test-on-values ()
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (:truncate t)
                (+ * 1)
                (concatenate 'string * "more")))
             '(2 "sdfmore")))
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (:truncate t)
                (+ * 1)
                (concatenate 'string * "more")))
             '(2 "sdfmore")))
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (:truncate nil)
                (+ * 1)
                (concatenate 'string * "more")))
             '(2 "sdfmore" 3)))
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (+ * 1)
                (concatenate 'string * "more")))
             '(2 "sdfmore" 3)))
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (:on-length-mismatch #'identity)))
             '(1 "sdf" 3)))
  (is (equal (multiple-value-list
              (on-values (values 1 "sdf" 3)
                (:on-length-mismatch (lambda (values)
                                       (subseq values 0 2)))))
             '(1 "sdf")))
  (signals (error :pred "Options must come first.")
    (eval '(on-values 1
            t
            (:on-length-mismatch #'identity)))))

(deftest test-with-retry ()
  (with-test (with-retry/catch)
    (let ((n 0)
          (m 0))
      (is (eql (try::with-retry/catch (:catch 'retry :on-retry (incf m))
                 (incf n)
                 (when (= n 1)
                   (throw 'retry nil))
                 7)
               7))
      (is (= n 2))
      (is (= m 1)))))

(deftest test-lambda-list-to-arglist-form ()
  (is (equal (try::lambda-list-to-arglist-form '(a b c))
             '(list a b c)))
  (flet ((roundtrip (lambda-list arglist)
           (multiple-value-bind (arglist-form new-lambda-list)
               (try::lambda-list-to-arglist-form lambda-list)
             (is (equal (apply (try::without-compiler-notes
                                 (compile nil
                                          `(lambda ,new-lambda-list
                                             ,arglist-form)))
                               arglist)
                        arglist)))))
    (roundtrip '(x &optional (o nil op) &rest r &key ((:k k) nil kp))
               '(0 1 :k 3))
    (roundtrip '(&optional o &key k l)
               '(0 :k 1 :l 2))
    (roundtrip '(&optional x (y nil y-p) (z t))
               '())))
