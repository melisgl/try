(in-package :try)

(in-readtable pythonic-string-syntax)

(defsection @try/count (:title "Counting Events")
  """TRIALs have a counter for each category in *CATEGORIES*. When an
  EVENT is recorded by TRY and its type matches *COUNT*, the counters
  of all categories matching the event type are incremented in the
  CURRENT-TRIAL. When a trial finishes and a VERDICT is recorded, the
  trial's event counters are added to that of its parent's (if any).
  The counts are printed with VERDICTs (see @TRY/PRINT).

  If both *COUNT* and *CATEGORIES* are unchanged from the their
  default values, then only LEAF events are counted, and we get
  separate counters for ABORT*, UNEXPECTED-FAILURE,
  UNEXPECTED-SUCCESS, SKIP, EXPECTED-FAILURE, and EXPECTED-SUCCESS.

  ```
  (let ((*debug* nil))
    (with-test (outer)
      (with-test (inner)
        (is t))
      (is t)
      (is nil)))
  .. OUTER
  ..   INNER
  ..     ⋅ (IS T)
  ..   ⋅ INNER ⋅1
  ..   ⋅ (IS T)
  ..   ⊠ (IS NIL)
  .. ⊠ OUTER ⊠1 ⋅2
  ..
  ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅2>
  ```

  As the above example shows, EXPECTED-VERDICT-SUCCESS and
  EXPECTED-RESULT-SUCCESS are both marked with `"⋅"`, but only
  EXPECTED-RESULT-SUCCESS is counted due to *COUNT* being LEAF.
  """)

;;;; This is just the COUNTER. The actual counting is implemented in
;;;; %COLLECTOR due to having to keep counts for non-collected events.

(defstruct (counter (:constructor %make-counter)
                    (:copier nil))
  counts
  categories)

(defun make-counter (categories)
  (%make-counter :counts (make-array (length categories)
                                     :element-type 'fixnum
                                     :initial-element 0)
                 :categories categories))

(defun bump-counter (counter event)
  (let ((categories (counter-categories counter))
        (counts (counter-counts counter)))
    (loop for category in categories
          for i upfrom 0
          do (let ((category-type (first category)))
               (when (typep event category-type)
                 (incf (aref counts i))))))
  counter)

(defun add-to-counter (counter1 counter2)
  (assert (equal (counter-categories counter1)
                 (counter-categories counter2)))
  (let ((counts1 (counter-counts counter1))
        (counts2 (counter-counts counter2)))
    (map-into counts1 #'+ counts1 counts2)))

(defun reset-counter (counter)
  (fill (counter-counts counter) 0))

(defun copy-counter (counter)
  (%make-counter :counts (copy-seq (counter-counts counter))
                 :categories (counter-categories counter)))
