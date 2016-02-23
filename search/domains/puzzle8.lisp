;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The 8-Puzzle Problem

;;; In this implementation of the 8-puzzle we have a mix of priorities
;;; between efficiency and simplicity.  We restrict ourselves to the
;;; 8-puzzle, instead of the general n-puzzle.  The representation of
;;; states is not the obvious one (a 3x3 array), but it is both
;;; efficient and fairly easy to manipulate.  We represent each tile
;;; as an integer from 0 to 8 (0 for the blank).  We also represent
;;; each square as an integer from 0 to 8, arranged as follows:
;;; <PRE>
;;;     0 1 2
;;;     3 4 5
;;;     6 7 8
;;; </PRE>
;;; Finally, we represent a state (i.e., a complete puzzle) as the sum
;;; of the tile numbers times 9 to the power of the tile's square number.
;;; For example, the goal state from page 63:
;;; <PRE> 
;;;     1 2 3                          1*9^0 + 2*9^1 + 3*9^2
;;;     8 . 4  is represented by:    + 8*9^3 + 0*9^4 + 4*9^5
;;;     7 6 5                        + 7*9^6 + 6*9^7 + 5*9^8 = 247893796
;;; </PRE>
;;; We represent actions with the four symbols <, >, ^, V to stand for
;;; moving the blank tile left, right, up and down, respectively.  The
;;; heuristic functions implicitly refer to the goal, *8-puzzle-goal*.
;;; Call the function USE-8-PUZZLE-GOAL to change the goal state if
;;; you wish.

(defvar *8-puzzle-goal* "To be defined later")

(defstructure (8-puzzle-problem 
	       (:include problem
			 (initial-state (random-8-puzzle-state))
			 (goal *8-puzzle-goal*)))
  "The sliding tile problem known as the 8-puzzle.")

(defmethod successors ((problem 8-puzzle-problem) state)
  "Generate the possible moves from an 8-puzzle state."
  (let ((blank (blank-square state))
	(result nil))
     (for each (action destination) in (8-puzzle-legal-moves blank) do
	  (push (cons action (move-blank state blank destination)) result))
     result))

(defmethod h-cost ((problem 8-puzzle-problem) state)
  "Manhatten, or sum of city block distances.  This is h_2 on [p 102]."
  (let ((sum 0))
   (for square = 0 to 8 do
	(let ((tile (8-puzzle-ref state square)))
	  (unless (= tile 0)
	    (incf sum (x+y-distance (8-puzzle-location square)
				    (8-puzzle-goal-location tile))))))
   sum))

(defun move-blank (state from to)
  "Move the blank from one square to another and return the resulting state.
  The FROM square must contain the blank; this is not checked."
  (+ state (* (8-puzzle-ref state to) (- (9-power from) (9-power to)))))

(defun blank-square (state)
  "Find the number of the square where the blank is."
  (for i = 0 to 8 do
       (when (= 0 (8-puzzle-ref state i)) (return i))))

(defun random-8-puzzle-state (&optional (num-moves 100) (state *8-puzzle-goal*))
  "Return a random state of the 8 puzzle."
  (for i = 1 to num-moves do
       (setf state (random-move-blank state)))
  state)

(defun random-move-blank (state)
  "Return a state derived from this one by a random move."
  (let ((blank (blank-square state)))
    (move-blank state blank (random-element (neighbors blank)))))

;;;; Representing the Board

(defun neighbors (square)
  "The squares that can be reached from a given square."
  (svref
   '#((1 3)     (0 2 4)     (1 5)
      (0 4 6)   (1 3 5 7)   (2 4 8)
      (3 7)     (4 6 8)     (7 5))
   square))

(defun 8-puzzle-legal-moves (square)
  "The moves that can be made when the blank is in a given square.
  This is a list of (direction destination-square) pairs."
  (svref
   '#(((> 1) (V 3))        ((< 0) (> 2) (V 4))        ((< 1) (V 5))
      ((^ 0) (> 4) (V 6))  ((^ 1) (< 3) (> 5) (V 7))  ((^ 2) (< 4) (V 8))
      ((^ 3) (> 7))        ((^ 4) (< 6) (> 8))        ((^ 5) (< 7)))
   square))

(defun 8-puzzle-location (square)
  "Return the (x y) location of a square number."
  (svref '#((0 2) (1 2) (2 2)
	    (0 1) (1 1) (2 1)
	    (0 0) (1 0) (2 0))
	 square))

;;;; Representing States

(defun 8-puzzle-state (&rest pieces)
  "Define a new state with the specified tiles."
  (assert (= 9 (length pieces)))
  (let ((sum 0))
   (for i = 0 to 8 do
	(incf sum (* (elt pieces i) (expt 9 i))))
   sum))

(defun 8-puzzle-ref (state square)
  "Return the tile that occupies the given square."
  (mod (floor state (9-power square)) 9))

(defun 8-puzzle-print (state &optional (stream t))
  (for i = 0 to 8 do
       (if (= 0 (mod i 3)) (terpri stream))
       (let ((tile (8-puzzle-ref state i)))
		  (format stream " ~A" (if (= tile 0) "." tile))))
  state)

(defun 8-puzzle-display-fn (node problem)
  (declare (ignore problem))
  (when *debugging*
    (format *debug-io* "~&Expanding:~%")
    (8-puzzle-print (node-state node) *debug-io*)))

;;;; Setting Up the Goal

(defvar *8-puzzle-goal-locations* (make-array 9)
  "A vector indexed by tile numbers, saying where the tile should be.")

(defun use-8-puzzle-goal (goal)
  "Define a new goal for the 8 puzzle."
  (setf *8-puzzle-goal* goal)
  (for square = 0 to 8 do
       (let ((tile (8-puzzle-ref goal square)))
	 (setf (svref *8-puzzle-goal-locations* tile)
	       (8-puzzle-location square))))
  goal)

(defun 8-puzzle-goal-location (tile)
  "Return the location where the tile should go."
  (svref *8-puzzle-goal-locations* tile))

(defun 9-power (n)
  "Raise 9 to the nth power, 0 <= n <= 9."
  ;; This measures about 8 times faster than (expt 9 i)
  (svref '#(1 9 81 729 6561 59049 531441 4782969 43046721 387420489) n))

(use-8-puzzle-goal
 (8-puzzle-state 1 2 3
		 8 0 4			; The goal on [p 63].
		 7 6 5))

;;;; Alternative Heuristic Function

(defun misplaced-tiles (state)
  "Number of misplaced tiles.  This is h_1 on [p 102]."
  (let ((sum 0))
   (for square = 0 to 8 do
	(when (misplaced-tile? state square) (incf sum)))
   sum))

(defun misplaced-tile? (state square)
  "Is the tile in SQUARE different from the corresponding goal tile?  
  Don't count the blank."
  (let ((tile (8-puzzle-ref state square)))
    (and (/= tile 0)
	 (/= tile (8-puzzle-ref *8-puzzle-goal* square)))))
