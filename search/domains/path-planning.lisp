;;;; Path Planning in 2 Dimensions with Convex Polygonal Obstacles

(defstructure (path-planning-problem (:include problem)
				     (:constructor create-path-planning-problem))
  "A problem involving moving among polygonal obstacles in 2D space.
  A state is the current vertex."
  scene)

(defun make-path-planning-problem (&key scene)
  "Define a constructor to build a problem, using the scene properly."
  (create-path-planning-problem 
   :scene scene
   :initial-state (scene-start scene)
   :goal (scene-goal scene)))

(defmethod successors ((problem path-planning-problem) v1)
  "Return a list of (action . state) pairs, where the state is another
  vertex that is visible from the current vertex v1, and the action is a 
  delta (dx dy) from  v1 to the new one."
  (let ((p1 (vertex-xy v1)))
    (mapcar #'(lambda (v2) (let ((p2 (vertex-xy v2)))
			     (cons (@ (- (xy-x p2) (xy-x p1)) 
				      (- (xy-y p2) (xy-y p1)))
				   v2)))
	    (vertices-visible-from v1 (path-planning-problem-scene problem)))))

(defmethod edge-cost ((problem path-planning-problem) node action vertex)
  "The cost of an action is its distance."
  (declare-ignore node vertex)
  (xy-distance '(0 0) action))

(defmethod h-cost ((problem path-planning-problem) vertex)
  "The heuristic cost is the straight-line distance to the goal."
  (xy-distance (vertex-xy vertex) (vertex-xy (problem-goal problem))))

;;;; Defining the Vertex, Line, Polygon and Scene Types

(defstructure vertex
  xy           ;; the xy point for the vertex
  c-neighbor   ;; neighbour in clockwise direction
  a-neighbor   ;; neighbour in anti-clockwise direction
  visible      ;; list of vertices visible from here
  )

(defmethod print-structure ((v vertex) stream)
  (format stream "#<V ~D,~D>" (xy-x (vertex-xy v)) (xy-y (vertex-xy v))))

(defstructure line
  xy1 xy2)

(defstructure polygon
  vertices n)

(defstructure scene
  polygons			; polygons comprising scene
  start				; vertex for start 
  goal				; vertex for goal
  )

;;; Functions for testing whether one vertex is visible from another

(defun vertices-visible-from (v1 scene)
  "Find all the vertices that can be seen from this vertex."
  ;; When you find them, cache them under the vertex-visible slot.
  (or (vertex-visible v1)
      (setf (vertex-visible v1) (vertices-in-view v1 scene))))
	    
(defun vertices-in-view (v scene)
  "Find all the other vertices that can be seen from v."
  (delete v
   (let ((result nil))
    (for each poly in (scene-polygons scene) do
	 (cond ((member v (polygon-vertices poly))
		(push (vertex-c-neighbor v) result)
		(push (vertex-a-neighbor v) result))
	       (t (for each v2 in (polygon-vertices poly) do
		       (when (visible-p (vertex-xy v) (vertex-xy v2) scene)
			 (push v2 result))))))
    result)))

(defun visible-p (xy1 xy2 scene)
  "Predicate; return t iff xy1 is visible from xy2."
  (let ( (line (make-line :xy1 xy1 :xy2 xy2)) )
    (dolist (poly (scene-polygons scene) t)
      (if (line-intersects-poly? line poly) (return nil)))))

(defun line-intersects-poly? (line poly)
  "Predicate; return t iff line intersects poly."
  (dolist (v1 (polygon-vertices poly) nil)
    (let ((v2 (vertex-c-neighbor v1)))
      (if (intersects line 
                      (make-line :xy1 (vertex-xy v1) :xy2 (vertex-xy v2)))
	  (return t)))))

(defun intersects (l1 l2)   
;;; l1 is line ab; l2 is line cd
;;; assume the lines cross at alpha a + (1-alpha) b, 
;;;     also known as beta c + (1-beta) d
;;; line segments intersect if 0<alpha,beta<1 unless they're parallel
  (let* ((a (line-xy1 l1))
	 (b (line-xy2 l1))
	 (c (line-xy1 l2))
	 (d (line-xy2 l2))
	 (xa (xy-x a)) (ya (xy-y a))
	 (xb (xy-x b)) (yb (xy-y b))
	 (xc (xy-x c)) (yc (xy-y c))
	 (xd (xy-x d)) (yd (xy-y d))
	 (q (- (* (- xa xb) (- yc yd))
	       (* (- ya yb) (- xc xd)))))
    (unless (= 0 q)
      (let ((alpha (/ (- (* (- xd xb) (- yc yd))
			 (* (- yd yb) (- xc xd)))
		      q))
	     (beta (/ (- (* (- xd xb) (- ya yb))
			   (* (- yd yb) (- xa xb)))
		         q)))
	(and (< 0 alpha 1) (< 0 beta 1))))))


;;;; Code for constructing the scene data structure

(defun create-scene (&key start goal polygons)
  "START and GOAL are xy points; polygons is a list of lists of vertices."
  (let ((polys (mapcar #'create-polygon 
		       (list* (list start) (list goal) polygons))))
    (make-scene :start (first (polygon-vertices (first polys)))
		:goal (first (polygon-vertices (second polys)))
		:polygons polys)))

(defun create-polygon (points)
  ;; Assumes that points are given in anticlockwise order (or in order, anyway)
  (let* ((vertices (mapcar #'(lambda (xy) (make-vertex :xy xy)) points))
	 (poly (make-polygon :vertices vertices)))
    (setf (polygon-n poly) (length vertices))
    (dolist (v vertices)
      (let ((v2 (or (second (member v vertices)) (first vertices))))
	(setf (vertex-a-neighbor v) v2)
	(setf (vertex-c-neighbor v2) v)))
    poly))      
    
;;;; Specific scene, shown as Figure 4.17 [p 120]

(defparameter *scene-4.17*
  (create-scene 
   :start '(112 660) :goal '(353 573)
   :polygons
   '(((220 616) (220 666) (251 670) (272 647))
     ((341 655) (359 667) (374 651) (366 577))
     ((311 530) (311 559) (339 578) (361 560) (361 528) (336 516))
     ((105 628) (151 670) (180 629) (156 577) (113 587))
     ((118 517) (245 517) (245 557) (118 557))
     ((280 583) (333 583) (333 665) (280 665))
     ((252 594) (290 562) (264 538))
     ((198 635) (217 574) (182 574))
     ))
  "The scene in Figure 4.17 [p 120] with 8 obstacles.")
