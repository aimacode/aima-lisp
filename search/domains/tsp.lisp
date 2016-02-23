;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: tsp.lisp

;;;; The Travelling Salesperson Problem (TSP)

;;; Find a tour: a path that visits every city exactly once, and returns to
;;; the starting city.  The shorter the total distance, the better.  This
;;; builds on the map data structure defined in route-finding.lisp.  It
;;; assumes that the map is a complete graph: there is a path from every city
;;; to every other city.
;;;
;;; Note: the TSP is NP complete in the general case, but there are some good
;;; algorithms for finding approximate solutions, particularly when the
;;; triangle inequality is satisfied (that the path from A->C is always
;;; shorter than A->B->C).  Many of these algorithms are based on the idea of
;;; building a minimum spanning tree, converting it into a tour, and perhaps
;;; modifying it.  We don't go into that here (because we are more interested
;;; in hooking up to the general search procedures than in special-purpose
;;; algorithms), but note that our tsp-h heuristic function is a relaxed
;;; version of a minimum spanning tree.

(defstructure (tsp-problem (:include problem) 
			   (:constructor create-tsp-problem))
  (map nil))

(defun make-tsp-problem (&key (map (random-tsp-map)) 
			      (start (city-name (first map))))
  "Constructor for TSP problems.  The map must be a complete graph."
  (check-tsp-map? map)
  (create-tsp-problem 
   :initial-state (make-tsp :visited (list start)
			    :to-visit (remove start (mapcar #'city-name map)))
   :map map))

(defmethod edge-cost ((problem tsp-problem) node action state)
  (declare (ignore action))
  (road-distance (find-city (tsp-city-name (node-state node))
			    (tsp-problem-map problem))
		 (tsp-city-name state)))

(defmethod h-cost ((problem tsp-problem) state)
  "A lower bound on the cost is the distance to ???"
  (let ((to-visit (tsp-to-visit state))
	(map (tsp-problem-map problem)))
    (+ (nearest-neighbor-distance (tsp-city-name state) to-visit map)
       (nearest-neighbor-distance (tsp-start state) to-visit map)
       (path-lower-bound to-visit map))))

(defmethod successors ((problem tsp-problem) state)
  "Return a list of (action . state) pairs.  Actions are just the name of
  the city to go to.  You can only go to a city you haven't visited yet,
  unless you've visited them all, in which case you can only go back home."
  (if (null (tsp-to-visit state))
      (list (cons (tsp-start state)
		  (make-tsp :to-visit nil
			    :visited (cons (tsp-start state)
					   (tsp-visited state)))))
    ;; This is similar to the method for route-finding-problem
    (let ((city (find-city (tsp-city-name state) (tsp-problem-map problem)))
	  (result nil))
        (for each pair in (city-neighbors city) do
	     (let ((next (first pair)))
	       (when (member next (tsp-to-visit state))
		 (push (cons next
				(make-tsp
				 :visited (cons next (tsp-visited state))
				 :to-visit (remove
					    next (tsp-to-visit state))))
		       result))))
	result)))

(defmethod goal-test ((problem tsp-problem) state)
  "The goal is to leave no unvisited cities and get back to start."
  (and (null (tsp-to-visit state))
       (eql (tsp-city-name state) 
	    (tsp-city-name (problem-initial-state problem)))))

(defstruct (tsp (:type list))
  "A state for a TSP problem lists cities visited, and remaining to see."
  (visited nil)			; List of names of cities visited so far
  (to-visit nil)		; Set of names of cities left to visit
  )

;;;; Auxiliary Functions

(defun nearest-neighbor-distance (name candidate-names map)
  "Find among the CANDIDATE-NAMES of cities, the one that is closest to
  city NAME, and return the distance to it."
  (if (null candidate-names)
      0
    (let ((city (find-city name map))
	  (distance infinity))
       (for each other-name in candidate-names do
	    (unless (eq other-name name)
	      (setf distance (min distance (road-distance city other-name)))))
       distance)))

(defun path-lower-bound (city-names map)
  "Find a lower bound for a path through these cities."
  ;; Each city must be connected to a next one, for n-1 links for n cities.
  ;; A lower bound is the sum of the shortest links for each city but first.
  (let ((sum 0))
   (for each name in (rest city-names) do
	(incf sum (nearest-neighbor-distance name city-names map)))
   sum))

(defun random-tsp-map (&key (n-cities 6))
  (random-route-map :n-cities n-cities :min-roads (- n-cities 1)
			       :max-roads (- n-cities 1)))

(defun check-tsp-map? (map)
  (for each city in map do
       (when (/= (length (city-neighbors city)) (- (length map) 1))
	 (error "This map can't be used for a travelling salesperson problem ~
                because ~A is not connected to every other city."
		(city-name city)))))

(defun tsp-city-name (tsp-state)
  "The current city: the last one visited."
  ;; We store the cities visited in reverse order, so take the first one
  (first (tsp-visited tsp-state)))

(defun tsp-start (tsp-state)
  (last1 (tsp-visited tsp-state)))

