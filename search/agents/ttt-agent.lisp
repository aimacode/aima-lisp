
;;;; An  Agent for Playing Tic-Tac-Toe

(defstructure (alpha-beta-ttt-agent 
	       (:include game-agent
		(algorithm #'(lambda (state game) 
			       (alpha-beta-decision state game #'ttt-eval)))))
  "A game-playing agent that uses ttt-eval to do alpha-beta search.")

(defun ttt-eval (state)
  "Evaluate a TTT board on a scale from -1 to +1."
  ;; This is a rather poor evaluation function.
  ;; Note that it doesn't even pay attention to K.
  ;; We just count the number of blank squares next to each player.
  ;; The more of these, the better.
  (let* ((board (game-state-board state))
	 (players (game-state-players state))
	 (values (make-list (length players) :initial-element 0))
	 (n (array-dimension board 0)))
    (dotimes (x n)
      (dotimes (y n)
	(when (eq (aref board x y) '-)
	  (for each delta in '((1 0) (0 1) (-1 0) (0 -1)) do
	       (let* ((neighbor (xy-add delta (@ x y)))
		      (piece (when (inside neighbor n n)
			       (aref board (xy-x neighbor) (xy-y neighbor)))))
		 (unless (member piece '(- nil))
		   ;; You get points for having your piece neighboring an empty
		   (incf (elt values (position piece players)) 0.001)))))))
    values))
