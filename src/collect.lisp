(in-package :try)

(defsection @collect (:title "Collecting Events")
  "When an EVENT is recorded and the type of the EVENT matches the
  COLLECT type argument of TRY, then a corresponding object is pushed
  onto CHILDREN of the CURRENT-TRIAL for subsequent @RERUN or @REPLAY.

  In particular, if the matching event is a LEAF, then the event
  itself is collected. If the matching event is a TRIAL-EVENT, then
  [VERDICT][(reader trial)] of its [TRIAL][(reader trial-event)] is
  collected. Furthermore, trials which collected anything are always
  collected by their parent.

  By default, both implicit and explicit calls to TRY collect the
  UNEXPECTED (see *COLLECT* and *TRY-COLLECT*), and consequently all
  the enclosing trials."
  (children (reader trial)))

(defstruct %collector
  count-type
  collect-type)

(defun %count-and-collect-event (collector event)
  (etypecase event
    (leaf (count-and-collect-leaf collector event))
    ;; TRIAL-START is also matched when the VERDICT is available.
    (trial-start (count-trial-start event (%collector-count-type collector)))
    (verdict (count-and-collect-verdict collector event))))

(defun count-and-collect-leaf (collector leaf)
  (when *trial*
    (let* ((count-type (%collector-count-type collector))
           (collect-type (%collector-collect-type collector))
           (collectp (safe-typep leaf collect-type)))
      (cond (collectp
             (push leaf (slot-value *trial* 'children))
             (count-collected-leaf leaf count-type))
            (t
             (count-non-collected-leaf leaf count-type))))))

(defun count-and-collect-verdict (collector verdict)
  (let* ((trial (trial verdict))
         (count-type (%collector-count-type collector))
         (collect-type (%collector-collect-type collector))
         (collectp (or (children trial)
                       (safe-typep verdict collect-type)
                       (safe-typep (trial-start trial) collect-type))))
    (assert (eq *trial* trial))
    (let ((parent (parent trial)))
      (when parent
        (cond (collectp
               (push verdict (slot-value parent 'children))
               (count-collected-verdict verdict count-type))
              (t
               (count-non-collected-verdict verdict trial parent
                                            count-type)))))))

(defun count-collected-leaf (leaf count-type)
  (when (typep leaf count-type)
    (bump-counter (counter *trial*) leaf)))

(defun count-non-collected-leaf (leaf count-type)
  (when (typep leaf 'fail)
    (setf (slot-value *trial* 'has-non-collected-failed-child-p) t))
  (when (typep leaf count-type)
    (bump-counter (counter *trial*) leaf)
    (bump-counter (non-collected-counter *trial*) leaf)))

(defun count-trial-start (trial-start count-type)
  (let ((trial (trial trial-start)))
    (when (typep trial-start count-type)
      (bump-counter (counter trial) trial-start)
      (bump-counter (non-collected-counter trial) trial-start))))

(defun count-collected-verdict (verdict count-type)
  (let* ((trial (trial verdict))
         (parent (parent trial)))
    (assert verdict)
    (when parent
      (let ((parent-counter (counter parent))
            (counter (counter trial)))
        (when (typep verdict count-type)
          (bump-counter parent-counter verdict))
        (add-to-counter parent-counter counter)))))

(defun count-non-collected-verdict (verdict trial parent count-type)
  (when (typep verdict 'fail)
    (setf (slot-value parent 'has-non-collected-failed-child-p) t))
  (when (typep verdict count-type)
    (bump-counter (counter parent) verdict)
    (bump-counter (non-collected-counter parent) verdict))
  (add-to-counter (counter parent) (counter trial))
  (add-to-counter (non-collected-counter parent)
                  (non-collected-counter trial)))
