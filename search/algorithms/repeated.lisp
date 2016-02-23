;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/algorithms/repeated

;;;; Search Algorithms That Avoid Repeated States

;;; In this file we show algorithms that worry about repeated states.
;;; Here are the three ways to deal with repeated states, from [p 82]:

(defun eliminate-returns (nodes)
  "Get rid of nodes that return to the state they just came from,
  i.e., where the last two actions just undo each other."
  (remove-if #'return-node? nodes))

(defun eliminate-cycles (nodes)
  "Get rid of nodes that end in a state that has appeared before in the path."
  (remove-if #'looping-node? nodes))

(defun eliminate-all-duplicates (nodes node-table)
  "Get rid of all nodes that have been seen before in any path."
  (let ((result nil))
   (for each node in nodes do
	(let ((state (node-state node)))
	  (when (not (gethash state node-table))
	    (push node result))
	  (setf (gethash state node-table) node)))
   result))

;;; Here are examples of search algorithms that use these methods.  In
;;; retrospect, a better organization would have been to have GENERAL-SEARCH
;;; take two arguments, a problem and a strategy, where the strategy would
;;; have a queueing function and an expansion function as components.  That
;;; way, we wouldn't have EXPAND generate nodes that we are just going to
;;; throw away anyway.

(defun no-cycles-depth-first-search (problem)
  "Do depth-first search, but eliminate paths with repeated states."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-front old-q (eliminate-cycles nodes)))))

(defun no-returns-breadth-first-search (problem)
  "Do breadth-first search, but eliminate immediate returns to a prior state."
  (general-search problem
		  #'(lambda (old-q nodes)
		      (enqueue-at-end old-q (eliminate-returns nodes)))))
			      
(defun no-duplicates-breadth-first-search (problem)
  "Do breadth-first search, but eliminate all duplicate states."
  (let ((table (make-hash-table :test #'equal)))
    (general-search problem
		    #'(lambda (old-q nodes)
			(enqueue-at-end old-q (eliminate-all-duplicates
					       nodes table))))))

(defun A*-search (problem)
  "Search the nodes with the best f cost first.  If a node is ever reached by
  two different paths, keep only the better path."
  (general-search problem (make-eliminating-queuing-fn #'node-f-cost)))

(defun make-eliminating-queuing-fn (eval-fn)
  (let ((table (make-hash-table :test #'equal)))
    #'(lambda (old-q nodes)
	(enqueue-by-priority
	 old-q
	 (let ((result nil))
	  (for each node in nodes do
	       (let ((old-node (gethash (node-state node) table)))
		 (cond
		  ((null old-node)
		   ;; First time we've reached state; just return node
		   (setf (gethash (node-state node) table) node)
		   (push node result))
		  ((<= (funcall eval-fn old-node) (funcall eval-fn node))
		   ;; If the old node is better, discard the new node
		   nil)
		  (t;; Otherwise, discard the old node
		   (setf (node-expanded? old-node) t)
		   (setf (gethash (node-state node) table) node)
		   (push node result)))))
	  (nreverse result))
	 eval-fn))))


;;;; Auxiliary Functions

(defun looping-node? (node &optional (depth infinity))
  "Did this node's state appear previously in the path?"
  ;; Search up to DEPTH nodes deep in the path
  (let ((n (node-parent node)))
    (for i = 1 to depth do
	 (when (null n) (return nil))
	 (when (equal (node-state node) (node-state n)) (return t))
	 (setf n (node-parent n)))))

(defun return-node? (node)
  "Is this a node that returns to the state it just came from?"
  (looping-node? node 2))
