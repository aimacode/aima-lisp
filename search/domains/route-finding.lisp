;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/domains/route-finding

;;;; Find a Route Between Cities on a Map

(defstructure (route-finding-problem (:include problem
					       (initial-state 'A)
					       (goal 'B)))
  "The problem of finding a route from one city to another on a map.
  By default it is a random map.  A state in a route-finding problem is just 
  the name of the current city. Note that a more complicated version of this
  problem would augment the state with considerations of time, gas
  used, wear on car, tolls to pay, etc."
  (map (random-route-map)))

(defmethod successors ((problem route-finding-problem) city-name)
  "Return a list of (action . new-state) pairs.
  In this case, the action and the new state are both the name of the city."
  (let ((result nil))
   (for each pair in (city-neighbors (find-city city-name problem)) do
	(push (cons (first pair) (first pair)) result))
   result))

(defmethod edge-cost ((problem route-finding-problem) node action city)
  "The edge-cost is the road distance to the next city."
  (declare-ignore action)
  (road-distance (find-city (node-state node) problem) city))

(defmethod h-cost ((problem route-finding-problem) city-name)
  "The heuristic cost is the straight-line distance to the goal."
  (straight-distance (find-city city-name problem)
		     (find-city (problem-goal problem) problem)))

;;;; The City and Map data structures

(defstruct (city (:type list))
  "A city's loc (location) is an (x y) pair.  The neighbors slot holds
  a list of (city-name . distance-along-road) pairs.  Be careful to 
  distinguish between a city name and a city structure."
  name loc neighbors)

(defun road-distance (city1 city-name2)
  "The distance along the road between two cities.  The first is a city 
  structure, the second just the name of the intended destination."
  (if (eq (city-name city1) city-name2)
      0
    (cdr (assoc city-name2 (city-neighbors city1)))))

(defun straight-distance (city1 city2)
  "Distance between two cities on a straight line (as the crow flies)."
  ;; We round this to the nearest integer, just to make things easier to read
  (round (xy-distance (city-loc city1) (city-loc city2))))

(defun find-city (name map)
  "Look up the city on the map, and return its information."
  (if (problem-p map) (setf map (route-finding-problem-map map)))
  (assoc name map))

(defun random-route-map (&key (n-cities 10) (width 100) (height 100)
			      (min-roads 2)
			      (max-roads (min n-cities (+ min-roads 3))))
  "Return a random map with n-cities in it, and some roads between them.
  Each city is connected to between MIN-ROADS and MAX-ROADS other cities.
  The default is from 2 to 5.  The road between any two cities has a length 
  of 1 to 1.5 times the straight-line distance between them."
  ;; First build the cities
  (let ((map nil))
    (for i = 1 to n-cities do
	 (push (make-city :name (number->name i) :neighbors nil
			  :loc (@ (random width) (random height)))
	       map))
    ;; Now lay down the roads
    (for each city in map do
	 (let* ((n-roads (max 0 (- (random-integer min-roads max-roads)
				   (length (city-neighbors city)))))
		(candidates
		 (sort (copy-list map) #'<
		       :key #'(lambda (city2)
				(straight-distance city city2)))))
	   (while (and candidates (> n-roads 0)) do
		(let ((city2 (pop candidates)))
		  (when (and city2 (not (eq city city2))
			     (not (assoc (city-name city2)
					 (city-neighbors city))))
			(decf n-roads)
			(build-road city city2))))))
    map))

(defun build-road (city1 city2)
  "Construct a road between two cities."
  (let* ((distance (straight-distance city1 city2))
	 (road-distance (round (* (+ 1.0 (random 0.5)) distance))))
    (push (cons (city-name city1) road-distance) (city-neighbors city2))
    (push (cons (city-name city2) road-distance) (city-neighbors city1))))

(defun number->name (i)
  "Turn an integer into a symbol.  1-26 go to A-Z; beyond that use Ci"
  (if (<= 1 i 26)
      (aref '#(0 a b c d e f g h i j k l m n o p q r s t u v w x y z) i)
    (intern (format nil "C~D" i))))

;;;; The Romanian Map

(defparameter *romania-map*
  '(
    (Arad       ( 91 492) ((Zerind . 75) (Sibiu . 140) (Timisoara . 118)))
    (Bucharest	(400 327) ((Fagaras . 211) (Pitesti . 101) (Giurgiu . 90)
			   (Urziceni . 85)))
    (Craiova	(253 288) ((Dobreta . 120) (Rimnicu . 146) (Pitesti . 138)))
    (Dobreta	(165 299) ((Mehadia . 75) (Craiova . 120)))
    (Eforie	(562 293) ((Hirsova . 86)))
    (Fagaras	(305 449) ((Sibiu . 99) (Bucharest . 211)))
    (Giurgiu	(375 270) ((Bucharest . 90)))
    (Hirsova	(534 350) ((Urziceni . 98) (Eforie . 86)))
    (Iasi	(473 506) ((Neamt . 87) (Vaslui . 92)))
    (Lugoj	(165 379) ((Timisoara . 111) (Mehadia . 70)))
    (Mehadia	(168 339) ((Lugoj . 70) (Dobreta . 75)))
    (Neamt	(406 537) ((Iasi . 87)))
    (Oradea	(131 571) ((Zerind . 71) (Sibiu . 151)))
    (Pitesti	(320 368) ((Rimnicu . 97) (Craiova . 138) (Bucharest . 101)))
    (Rimnicu	(233 410) ((Sibiu . 80) (Pitesti . 97) (Craiova . 146)))
    (Sibiu	(207 457) ((Arad . 140) (Oradea . 151) (Fagaras . 99)
			   (Rimnicu . 80)))
    (Timisoara	( 94 410) ((Arad . 118) (Lugoj . 111)))
    (Urziceni	(456 350) ((Bucharest . 85) (Hirsova . 98) (Vaslui . 142)))
    (Vaslui	(509 444) ((Iasi . 92) (Urziceni . 142)))
    (Zerind	(108 531) ((Arad . 75) (Oradea . 71)))
    )
  "A representation of the map in Figure 4.2 [p 95].
  But note that the straight-line distances to Bucharest are NOT the same.")

(defstructure (romanian-problem (:include route-finding-problem
					  (initial-state 'Arad)
					  (goal 'Bucharest)
					  (map *romania-map*)))
  "A route-finding problem on the Romania map, with random start and goal.")
