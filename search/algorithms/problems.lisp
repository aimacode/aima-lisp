;; -*- Mode: Lisp -*-

;;;; Defining Problems

(defstructure problem
  "A problem is defined by the initial state, and the type of problem it is.
  We will be defining subtypes of PROBLEM later on.  For bookkeeping, we
  count the number of nodes expanded.  Note that the three other fields from
  the book's definition [p 60] have become generic functions; see below."
  (initial-state (required)) ; A state in the domain
  (goal nil)                 ; Optionally store the desired state here.
  (num-expanded 0)           ; Number of nodes expanded in search for solution.
  (iterative? nil)           ; Are we using an iterative algorithm?
  )

;;; When we define a new subtype of problem, we need to define a SUCCESSORS
;;; method. We may need to define methods for GOAL-TEST, H-COST, and
;;; EDGE-COST, but they have default methods which may be appropriate.

(defmethod successors ((problem problem) state)
  "Return an alist of (action . state) pairs, reachable from this state."
  (declare-ignore state)
  (error "You need to define a SUCCESSORS method for ~A" problem))

(defmethod goal-test ((problem problem) state)
  "Return true or false: is this state a goal state?  This default method
  checks if the state is equal to the state stored in the problem-goal slot.
  You will need to define your own method if there are multiple goals, or if
  you need to compare them with something other than EQUAL."
  (declare-ignore state)
  (equal state (problem-goal problem)))

(defmethod h-cost ((problem problem) state) 
  "The estimated cost from state to a goal for this problem.  
  If you don't overestimate, then A* will always find optimal solutions.
  The default estimate is always 0, which certainly doesn't overestimate."
  (declare (ignore state))
  0)

(defmethod edge-cost ((problem problem) node action state)
  "The cost of going from one node to the next state by taking action.
  This default method counts 1 for every action.  Provide a method for this if 
  your subtype of problem has a different idea of the cost of a step."
  (declare-ignore node action state)
  1)

;;;; Manipulating Nodes

(defstructure node
  "Node for generic search.  A node contains a state, a domain-specific
  representation of a point in the search space.  A node also contains 
  bookkeeping information such as the cost so far (g-cost) and estimated cost 
  to go (h-cost). [p 72]"
  (state (required))        ; a state in the domain
  (parent nil)              ; the parent node of this node
  (action nil)              ; the domain action leading to state
  (successors nil)          ; list of sucessor nodes
  (unexpanded nil)          ; successors not yet examined (SMA* only)
  (depth 0)                 ; depth of node in tree (root = 0)
  (g-cost 0)                ; path cost from root to node
  (h-cost 0)                ; estimated distance from state to goal
  (f-cost 0)                ; g-cost + h-cost
  (expanded? nil)           ; any successors examined?
  (completed? nil)          ; all successors examined? (SMA* only)
  )

(defun expand (node problem)
  "Generate a list of all the nodes that can be reached from a node."
  ;; Note the problem's successor-fn returns a list of (action . state) pairs.
  ;; This function turns each of these into a node.
  ;; If a node has already been expanded for some reason, then return no nodes,
  ;; unless we are using an iterative algorithm.
  (unless (and (node-expanded? node) (not (problem-iterative? problem)))
    (setf (node-expanded? node) t)
    (incf (problem-num-expanded problem))
    (display-expand problem node)
    (let ((nodes nil))
      (for each (action . state) in (successors problem (node-state node)) do
	   (let* ((g (+ (node-g-cost node) 
			(edge-cost problem node action state)))
		  (h (h-cost problem state)))
	     (push
	      (make-node 
	       :parent node :action action :state state
	       :depth (1+ (node-depth node)) :g-cost g :h-cost h
	       ;; use the pathmax equation [p 98] for f:
	       :f-cost (max (node-f-cost node) (+ g h)))
	      nodes)))
      nodes)))

(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (let ((h (h-cost problem (problem-initial-state problem))))
    (make-node :state (problem-initial-state problem)
	       :h-cost h :f-cost h)))

;;;; Manipulating Solutions

;;; Solutions are represented just by the node at the end of the path.  The
;;; function SOLUTION-ACTIONS returns a list of actions that get there.  It
;;; would be problematic to represent solutions directly by this list of
;;; actions, because then we couldn't tell a solution with no actions from a
;;; failure to find a solution. 

(defun solution-actions (node &optional (actions-so-far nil))
  "Return a list of actions that will lead to the node's state."
  (cond ((null node) actions-so-far)
	((null (node-parent node)) actions-so-far)
	(t (solution-actions (node-parent node)
			     (cons (node-action node) actions-so-far)))))

(defun solution-nodes (node &optional (nodes-so-far nil))
  "Return a list of the nodes along the path to the solution."
  (cond ((null node) nodes-so-far)
	(t (solution-nodes (node-parent node)
			   (cons node nodes-so-far)))))

(defun solve (problem &optional (algorithm 'A*-search))
  "Print a list of actions that will solve the problem (if possible).
  Return the node that solves the problem, or nil."
  (setf (problem-num-expanded problem) 0)
  (let ((node (funcall algorithm problem)))
    (print-solution problem node)
    node))

(defun print-solution (problem node)
  "Print a table of the actions and states leading up to a solution."
  (if node
      (format t "~&Action ~20T State~%====== ~20T =====~%")
    (format t "~&No solution found.~&"))
  (for each n in (solution-nodes node) do
       (format t "~&~A ~20T ~A~%"
	       (or (node-action n) "") (node-state n)))
  (format t "====== ~20T =====~%Total of ~D node~:P expanded."
	  (problem-num-expanded problem))
  node)

;;;; Comparing Algorithms

(defun compare-search-algorithms (problem-fn algorithms &key (n 10))
  "Run each algorithm on N problems (as generated by problem-fn)
  and compare the results for nodes expanded and for path cost."
  (let ((random-state (make-random-state t)))
    (format t "~&Solved  Cost  Length  Nodes  Algorithm")
    (format t "~&====== ====== ====== ======= =========")
    (for each algorithm in algorithms do
	 (let ((g-cost 0)
	       (num-expanded 0)
	       (num-solved 0)
	       (path-length 0)
	       (copy-of-random-state (make-random-state random-state)))
	   (for i = 1 to n do
		(let* ((problem (let ((*random-state* copy-of-random-state))
				  (funcall problem-fn)))
		       (solution (funcall algorithm problem)))
		  (incf num-expanded (problem-num-expanded problem))
		  (when solution
		    (incf num-solved)
		    (incf path-length (node-depth solution))
		    (incf g-cost (node-g-cost solution)))))
	   (let ((M (if (= num-solved 0) 1 num-solved)))
	     (format t "~&~5D  ~6,1F ~6,1F ~7,1F ~A~%"
		     num-solved (/ g-cost M) (/ path-length M)
		     (/ num-expanded N) algorithm)))))
  (values))

;;;; Printing

(defmethod print-structure ((node node) stream) 
  (format stream "#<NODE f(~D) = g(~D) + h(~D) state:~A>" (node-f-cost node)
	  (node-g-cost node) (node-h-cost node) (node-state node)))

(defmethod display-expand ((problem problem) node)
  "Possibly print information when a node is expanded."
  (dprint 'expanding node))

