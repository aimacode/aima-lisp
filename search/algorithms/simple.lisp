;; -*- Mode: Lisp -*-

;;;; Simple Search Algorithms

;;; Here we define the GENERAL-SEARCH function, and then a set of
;;; search functions that follow specific search strategies.  None of
;;; these algorithms worries about repeated states in the search.

(defun general-search (problem queuing-fn)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The QUEUING-FN decides which
  nodes to look at first. [p 73]"
  (let ((nodes (make-initial-queue problem queuing-fn))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (goal-test problem (node-state node)) (RETURN node))
	  (funcall queuing-fn nodes (expand node problem)))))

(defun breadth-first-search (problem)
  "Search the shallowest nodes in the search tree first. [p 74]"
  (general-search problem #'enqueue-at-end))

(defun depth-first-search (problem)
  "Search the deepest nodes in the search tree first. [p 78]"
  (general-search problem #'enqueue-at-front))

(defun iterative-deepening-search (problem)
  "Do a series of depth-limited searches, increasing depth each time. [p 79]"
  (for depth = 0 to infinity do
       (let ((solution (depth-limited-search problem depth)))
	 (unless (eq solution :cut-off) (RETURN solution)))))

(defun depth-limited-search (problem &optional (limit infinity)
                                     (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond ((goal-test problem node) node)
        ((>= (node-depth node) limit) :cut-off)
        (t (for each n in (expand node problem) do
		(let ((solution (depth-limited-search problem limit n)))
		  (when solution (RETURN solution)))))))

;;;; Search Algorithms That Use Heuristic Information

(defun best-first-search (problem eval-fn)
  "Search the nodes with the best evaluation first. [p 93]"
  (general-search problem #'(lambda (old-q nodes) 
			      (enqueue-by-priority old-q nodes eval-fn))))

(defun greedy-search (problem)
  "Best-first search using H (heuristic distance to goal). [p 93]"
  (best-first-search problem #'node-h-cost))

(defun tree-a*-search (problem)
  "Best-first search using estimated total cost, or (F = G + H). [p 97]"
  (best-first-search problem #'node-f-cost))

(defun uniform-cost-search (problem)
  "Best-first search using the node's depth as its cost.  Discussion on [p 75]"
  (best-first-search problem #'node-depth))

;;;; Utility Function

(defun make-initial-queue (problem queuing-fn)
  (let ((q (make-empty-queue)))
    (funcall queuing-fn q (list (create-start-node problem)))
    q))
