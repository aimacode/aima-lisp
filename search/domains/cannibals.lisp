;; -*- Mode: Lisp; -*- 

;;;; The Missionaries and Cannibals Domain

(defstructure (cannibal-problem 
	       (:include problem (initial-state (make-cannibal-state)))) 
"The problem is to move M missionaries and C cannibals from one side
 of a river to another, using B boats that holds at most two people each,
 in such a way that the cannibals never outnumber the missionaries in
 any one place.  See [p 68].")

(defmethod goal-test ((problem cannibal-problem) state)
  "The goal is to have no missionaries or cannibals left on the first side."
  (= 0 (m1 state) (c1 state)))

(defmethod successors ((problem cannibal-problem) state)
  "Return a list of (action . state) pairs.  An action is a triple of the
  form (delta-m delta-c delta-b), where a positive delta means to move from
  side 1 to side 2; negative is the opposite.  For example, the action (1 0 1)
  means move one missionary and 1 boat from side 1 to side 2."
  (let ((pairs nil))
   (for each action in '((+1 0 +1) (0 +1 +1) (+2 0 +1) (0 +2 +1) (+1 +1 +1)
			 (-1 0 -1) (0 -1 -1) (-2 0 -1) (0 -2 -1) (-1 -1 -1)) do
	(let ((new-state (take-the-boat state action)))
	  (when (and new-state (not (cannibals-can-eat? new-state)))
	    (push (cons action new-state) pairs))))
   pairs))

(defstruct (cannibal-state (:conc-name nil) (:type list))
  "The state says how many missionaries, cannibals, and boats on each
  side.  The components m1,c1,b1 stand for the number of missionaries,
  cannibals and boats, respectively, on the first side of the river.
  The components m2,c2,b2 are for the other side of the river."
  ;; We need to represent both sides (rather than just one as on [p 68])
  ;; because we have generalized from 3+3 people to M+C.  Incidently, we
  ;; also generalized from 1 boat to B boats.
  (m1 3) (c1 3) (b1 1) (m2 0) (c2 0) (b2 0))

(defun take-the-boat (state action)
  "Move a certain number of missionaries, cannibals, and boats (if possible)."
  (destructuring-bind (delta-m delta-c delta-b) action
    (if (or (and (= delta-b +1) (> (b1 state) 0))
	    (and (= delta-b -1) (> (b2 state) 0)))
	(let ((new (copy-cannibal-state state)))
	  (decf (m1 new) delta-m) (incf (m2 new) delta-m)
	  (decf (c1 new) delta-c) (incf (c2 new) delta-c)
	  (decf (b1 new) delta-b) (incf (b2 new) delta-b)
	  (if (and (>= (m1 new) 0) (>= (m2 new) 0)
		   (>= (c1 new) 0) (>= (c2 new) 0))
	      new
	    nil))
      nil)))

(defun cannibals-can-eat? (state)
  "The cannibals feast if they outnumber the missionaries on either side."
  (or (> (c1 state) (m1 state) 0)
      (> (c2 state) (m2 state) 0)))

