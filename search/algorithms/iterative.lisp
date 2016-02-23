;;;; Iterative Improvement Search Algorithms

;;; Currently these do not do repeated-state checking.  Each takes a problem
;;; and returns two values: like all search algorithms, the first is a
;;; solution node or nil, but the second value will be the best node found
;;; so far, even if it is not a solution.  We will assume that all
;;; evaluations are costs (i.e., we're seeking minima).

;;;; Top Level Functions

(defun hill-climbing-search (problem 
			     &optional (stopping-criterion #'minimum-or-flat))
  "Search by picking the best successor according to heuristic h.
  Stops according to stopping-criterion."
  (let ((current (create-start-node problem))
	next)
    (loop
     (let ((successors (expand current problem)))
       (when successors
	 (setf next (the-smallest-random-tie #'node-h-cost successors)))
       (when (or (null successors)
		 (funcall stopping-criterion current next))
	 (return (values (goal-test problem current) current)))
       (setf current next)))))

(defun simulated-annealing-search (problem &optional
					   (schedule (make-exp-schedule)))
  "Like hill-climbing-search, except that we pick a next node randomly;
  if it is better, or if the badness of the next node is small and the
  'temperature' is large, then we accpet it, otherwise we ignore it.
  We halt when the temperature, TEMP, hits zero [p 113]."
  ;; Unlike [p 113], we keep track of successors to avoid generating them twice.
  ;; Also, we return the best node, rather than the current node
  (let* ((current (create-start-node problem))
	 (successors (expand current problem))
	 (best current)
	 next temp delta)
    (for time = 1 to infinity do
	 (setf temp (funcall schedule time))
	 (when (or (= temp 0) (null successors))
	   (RETURN (values (goal-test problem best) best)))
	 (when (< (node-h-cost current) (node-h-cost best))
	   (setf best current))
	 (setf next (random-element successors))
	 (setf delta (- (node-h-cost next) (node-h-cost current)))
	 (when (or (< delta 0.0) ; < because we are minimizing
		   (< (random 1.0) (exp (/ (- delta) temp))))
	   (setf current next
		 successors (expand next problem))))))

(defun random-restart-search (problem-fn &optional (n 10))
  "Random-restart hill-climbing repeatedly calls hill-climbing-search.
  PROBLEM-FN should return a problem with a random initial state.
  We look at N different initial states, and keep the best solution found."
  (let ((best-node nil))
    (for i = 1 to n do
	 (multiple-value-bind (solution node)
	     (hill-climbing-search (funcall problem-fn))
	   (declare (ignore solution))
	   (when (or (null best-node)
		     (< (node-h-cost node) (node-h-cost best-node)))
	     (setf best-node node))))
    best-node))

(defun hill-climbing-until-flat-n-times-search (problem &optional (n 4))
  "Do hill climbing, but stop after no improvement N times in a row."
  (hill-climbing-search problem (minimum-or-flat-n-times n)))

;;;; Auxiliary Functions

(defun local-minimum (current next)
  "Stop when the next state is worse than the current."
  (> (node-h-cost next) (node-h-cost current)))

(defun minimum-or-flat (current next)
  "Stop when the next state is no better than the current."
  (>= (node-h-cost next) (node-h-cost current)))

(defun minimum-or-flat-n-times (n)
  "Return a function that stops when no improvement is made N times in a row."
  (let ((times-in-a-row 0))
    #'(lambda (current next)
	(cond ((< (node-h-cost next) (node-h-cost current))
	       (setf times-in-a-row 0)
	       nil)
	      ((>= (incf times-in-a-row) n))))))

(defun CSP-termination (current next)
  (declare (ignore next))
  (CSP-goalp (node-state current)))

(defun make-exp-schedule (&key (k 20) (lambda 0.005) (limit 100))
  "Return an exponential schedule function with time limit."
  #'(lambda (time) (if (< time limit)
		       (* k (exp (- (* lambda time))))
		     0)))

  
