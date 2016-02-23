;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/domains/cognac.lisp

;;;; The Game of Cognac

;;; Definitions for a game of uncertain origin reputed to be played by
;;; bored cognac-tenders in the cellars.  Similar to Tic-Tac-Toe but
;;; instead of playing anywhere, one can only play directly above an
;;; existing mark or on the bottom row. 

(defstructure (cognac-game (:include ttt-game) 
			   (:constructor create-cognac-game))
  "Define an NxN tic-tac-toe-like game.  The object is to get K in a row.")

(defun make-cognac-game (&key (n 3) (k n) (players '(X O)))
  "Define an NxN Cognac game in which the object is to get K in a row."
  (create-cognac-game
   :n n :k k
   :initial-state (make-game-state
		   :board (make-array (list n n) :initial-element '-)
		   :players players)))


(defmethod legal-moves ((game cognac-game) state)
  "List all possible legal moves.  Like tic-tac-toe, except in each column
  you can only move to the lowest unoccupied square."
  (let ((board (game-state-board state)))
    ;; For each column (x position), the one (at most) possible move
    ;; is the lowest empty y position.
    (let ((moves nil))
     (dotimes (x (array-dimension board 0))
       (dotimes (y (array-dimension board 1))
	 (when (eq (aref board x y) '-)
	   (push (@ x y) moves)
	   (return) ; out of y loop
	   )))
     moves)))

;;; Everything else is inherited from ttt-game.

