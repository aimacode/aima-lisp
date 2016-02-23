;;; ida.lisp

;;;; Iterative Deepening A* (IDA*) Search

(defun tree-ida*-search (problem)
  "Iterative Deepening Tree-A* Search [p 107]."
  ;; The main loop does a series of f-cost-bounded depth-first
  ;; searches until a solution is found. After each search, the f-cost
  ;; bound is increased to the smallest f-cost value found that
  ;; exceeds the previous bound.  Note that the variables here are
  ;; local, not static as on [p 107].
  (setf (problem-iterative? problem) t)
  (let* ((root (create-start-node problem))
	 (f-limit (node-f-cost root))
	 (solution nil))
    (loop (multiple-value-setq (solution f-limit)
	    (DFS-contour root problem f-limit))
        (dprint "DFS-contour returned" solution "at" f-limit)
	(if (not (null solution)) (RETURN solution))
	(if (= f-limit infinity) (RETURN nil)))))

(defun DFS-contour (node problem f-limit)
  "Return a solution and a new f-cost limit."
  (let ((next-f infinity))
    (cond ((> (node-f-cost node) f-limit)
	   (values nil (node-f-cost node)))
	  ((goal-test problem (node-state node))
	   (values node f-limit))
	  (t (for each s in (expand node problem) do
		  (multiple-value-bind (solution new-f)
		      (DFS-contour s problem f-limit)
		    (if (not (null solution))
			(RETURN-FROM DFS-contour (values solution f-limit)))
		    (setq next-f (min next-f new-f))))
	     (values nil next-f)))))
	 

