;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/domains/ttt.lisp

;;;; The Game of Tic-Tac-Toe

;;; Generalized Tic-Tac-Toe, in which any number of players take turns
;;; placing marks on an NxN board, trying to get K marks in a row.  There
;;; are much more efficient representations than what we have chosen here,
;;; but this suffices to run an environment quickly.  If an agent wants to
;;; search a large number of possible game states, then the agent should use
;;; its own efficient representation.  After all, an agent's internal
;;; representation is independent of what's "actually" out there in the
;;; environment.

(defstructure (ttt-game (:include game) (:constructor create-ttt-game))
  "Define an NxN tic-tac-toe game in which the object is to get K in a row."
  n k)

(defun make-ttt-game (&key (n 3) (k n) (players '(X O)))
  "Define an NxN tic-tac-toe game in which the object is to get K in a row."
  (create-ttt-game
   :n n :k k
   :initial-state (make-game-state
		   :board (make-array (list n n) :initial-element '-)
		   :players players)))

(defmethod legal-moves ((game ttt-game) state)
  "List all possible legal moves."
  (let* ((board (game-state-board state))
	 (dims (array-dimensions board)))
    ;; Iterate over all squares; make moves in empty ones.
    (let ((moves nil))
     (dotimes (x (xy-x dims))
       (dotimes (y (xy-y dims))
	 (when (eq (aref board x y) '-)
	   (push (@ x y) moves))))
     moves)))

(defmethod make-move ((game ttt-game) state move)
  "Return the new state that results from making this move."
  (make-game-state
   :board (let ((new-board (copy-array (game-state-board state))))
	    (setf (aref new-board (xy-x move) (xy-y move))
		  (current-player state))
	    new-board)
   :players (left-rotate (game-state-players state))
   :scores (copy-list (game-state-scores state))
   :previous-move move))

(defmethod game-over? ((game ttt-game) state)
  "Checks if the last player to move made a complete row,
   column, or diagonal of length k, or if the board is full.
   If so, assign scores and return true; otherwise return nil."
  (let* ((n (ttt-game-n game))
	 (k (ttt-game-k game))
	 (board (game-state-board state))
	 (players (game-state-players state))
	 (x (first (game-state-previous-move state)))
	 (y (second (game-state-previous-move state)))
	 (previous (previous-player state)))
    (cond ((and x y
		(or (check-k-in-a-row board x y n k +1  0 previous)
		    (check-k-in-a-row board x y n k  0 +1 previous)
		    (check-k-in-a-row board x y n k -1 +1 previous)
		    (check-k-in-a-row board x y n k +1 +1 previous)))
	   (for each player in players do
		(setf (getf (game-state-scores state) player)
		      (if (eq player previous) +1 -1)))
	   'win)
	  ((not (find '- (array->vector board)))
	   'draw)
	  (t nil))))
	     
;;;; Auxiliary Functions

(defun check-k-in-a-row (board x y n k dx dy player)
  "Does player have k in a row, through (x y) in direction (+/-dx +/-dy)?"
  (>= (+ (count-pieces-in-direction board x y n (- dx) (- dy) player) 
	 (count-pieces-in-direction board x y n dx dy player)
	 -1) ; because the piece at (x y) gets counted twice
      k))

(defun count-pieces-in-direction (board x y n dx dy player)
  "Count player's pieces starting at (x y) going in direction (dx dy)."
  (if (and (< -1 x n) (< -1 y n) (eq (aref board x y) player))
      (+ 1 (count-pieces-in-direction board (+ x dx) (+ y dy)
				      n dx dy player))
    0))
