;;; sma.lisp
;;; Currently contains definition for a version of SMA* that operates on
;;; search trees (i.e., no repeated-state checking).
;;; [[Need to update to eliminate looping when memory is too small
;;; and to signal suboptimal solutions when appropriate.]]
;;; Although the basic algorithm is quite simple, the bookkeeping is not.

(defun tree-sma (problem &optional (memory-size 20)
			 &aux n
			      (start (create-start-node problem))
			      (q (make-search-tree start (node-f-cost start)))
			      (memory-used 1))

  (loop 
   (when (empty-tree q) (return nil))
   (setq n (deepest-least-leaf q))
   (when (goal-test problem n)
     (return n))
   (when (= (node-f-cost n) infinity) (return nil))
   (let ((s (tree-get-next-successor n q memory-size problem)))
     (when s
       (unless (node-unexpanded n)  ;;; n exhausted, drop from queue
	 (delete-element n q (node-f-cost n)))
       (incf memory-used)
       (insert-element s q (node-f-cost s))
       (when (> memory-used memory-size)
	 (tree-prune-open q)
	 (decf memory-used)))))
  )


;;; tree-get-next-successor returns the next successor of n, if any (else nil)
(defun tree-get-next-successor (n q memory-size problem &aux (next nil))
  (unless (node-expanded? n) 
    (setf (node-unexpanded n)
	  (if  (= (1+ (node-depth n)) memory-size)
	      (list 'done)
	    (nconc (expand n problem) (list 'done))))
    (setf (node-expanded? n) t))
  (unless (eq (car (node-unexpanded n)) 'done)
    (setq next (pop (node-unexpanded n)))
    (push next (node-successors n)))
  (unless (node-completed? n)
    (when (eq (car (node-unexpanded n)) 'done)  ;;; all successors examined 
      (pop (node-unexpanded n))
      (setf (node-completed? n) t)
      (tree-backup-f-cost n q t)))
  next)

;;; tree-backup-f-cost updates the f-cost for a node's ancestors as needed
(defun tree-backup-f-cost (node q &optional (was-open? nil) 
                                  &aux (current (node-f-cost node))
				       (least infinity)) 
  (when (node-completed? node)
    (dolist (s (node-successors node))
      (let ((v (node-f-cost s)))
        (when (< v least) (setq least v))))
    (dolist (s (node-unexpanded node))
      (let ((v (node-f-cost s)))
        (when (< v least) (setq least v))))
    (when (> least current)
      (when (or was-open? (openp node))  ;;; changing f value - re-order
        (delete-element node q current)
        (insert-element node q least))
      (setf (node-f-cost node) least)
      (let ((parent (node-parent node)))
        (when parent (tree-backup-f-cost parent q))))))


;;; tree-prune-open removes the worst node from the open list.
;;; The node is discarded from the open list, and its successors are
;;; dumped to recycle memory. If the parent was closed, it must be
;;; re-opened, with an updated f-cost (no need to do this until now
;;; because it wasn't on the open list anyway). Closed parent or not,
;;; the worstnode becomes an unexpanded successor of the parent. 

(defun tree-prune-open (q &aux (worstnode (shallowest-largest-leaf q))
                               (parent (node-parent worstnode)))
  (delete-element worstnode q (node-f-cost worstnode))
  (setf (node-successors worstnode) nil) ;;;actually free up memory
  (setf (node-expanded? worstnode) nil)

  (unless (node-unexpanded parent)   ;;;parent was closed - need to re-open
    (insert-element parent q (node-f-cost parent)))
  (tree-unexpand-successor worstnode parent))

(defun tree-unexpand-successor (successor parent)  
  (setf (node-unexpanded parent) 
	(nconc (node-unexpanded parent) (list successor)))
  (setf (node-successors parent)
	(delete successor (node-successors parent) :test #'eq)) 
  (when (node-completed? parent)
    (unless (node-successors parent)
      (setf (node-unexpanded parent) nil) ;;; reclaim space
      (setf (node-expanded? parent) nil)
      (setf (node-completed? parent) nil))))




(defun deepest-least-leaf (q)
  (the-biggest #'(lambda (n) (node-depth n)) (search-tree-node-value
					       (leftmost q)))) 

(defun shallowest-largest-leaf (q)
  (the-smallest-that 
    #'(lambda (n) (node-depth n))
    #'leafp
    (search-tree-node-value (rightmost q))))


(defun find-leaf (node &aux (s (node-successors node)))
  (if s (find-leaf (car s))
      node))

(defun leafp (n)
  (null (node-successors n)))

(defun openp (n)
  (or (not (node-expanded? n))
      (node-unexpanded n)))



