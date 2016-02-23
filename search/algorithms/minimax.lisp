;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/algorithms/minimax

;;;; Deciding What Move to Make in a Game by Minimax or Alpha-Beta Search

;;; The minimax decision procedure returns the optimal move in the game
;;; using exhaustive generation of the entire game tree.  Implementation
;;; uses the fact that the evaluation and utility functions return a list of
;;; values from the point of view of each player, with the "current" player
;;; first. Hence, rather than using #'min, we always use #'max for the
;;; current player.  A successor value is passed up the tree using
;;; right-rotation.  This works for any number of players.

;;; The notation "a+s" means an (action . state) pair.

;;;; Minimax

(defun minimax-decision (state game)
  "Return the best action, according to backed-up evaluation.
  Searches the whole game tree, all the way down to the leaves.
  This takes too much time for all but the simplest games,
  but it is guaranteed to produce the best action."
  (car (the-biggest
        #'(lambda (a+s) (first (right-rotate (minimax-value (cdr a+s) game))))
        (game-successors state game))))

(defun minimax-value (state game)
  (if (game-over? game state)
      (terminal-values state)
      (right-rotate
       (the-biggest
        #'(lambda (values) (first (right-rotate values)))
        (mapcar #'(lambda (a+s) (minimax-value (cdr a+s) game))
                (game-successors state game))))))

;;;; Minimax with Cutoff

(defun minimax-cutoff-decision (state game eval-fn limit)
  "Return the best action, according to backed-up evaluation down to LIMIT.
  After we search LIMIT levels seep, we use EVAL-FN to provide an estimate
  of the true value of a state; thus the action may not actually be best."
  (car (the-biggest
        #'(lambda (a+s) 
            (first (right-rotate 
                    (minimax-cutoff-value (cdr a+s) game eval-fn (- limit 1)))))
        (game-successors state game))))

(defun minimax-cutoff-value (state game eval-fn limit)
  (cond ((game-over? game state) (terminal-values state))
	((<= 0 limit) (funcall eval-fn state))
	(t (right-rotate
	    (the-biggest
	     #'(lambda (values) (first (right-rotate values)))
	     (mapcar #'(lambda (a+s) 
			 (minimax-cutoff-value (cdr a+s) game eval-fn 
					       (- limit 1)))
		     (game-successors state game)))))))


(defun game-successors (state game)
  "Return a list of (move . state) pairs that can be reached from this state."
  (mapcar #'(lambda (move) (cons move (make-move game state move)))
	  (legal-moves game state)))

(defun terminal-values (state)
  "Return the values of the state for each player."
  (mapcar #'(lambda (player) (getf (game-state-scores state) player))
	  (game-state-players state)))

;;;; Alpha-Beta Search

;;; The alpha-beta decision procedure returns the optimal move according to a
;;; limited-depth search using the evaluation function.  It returns the same
;;; action as minimax-cutoff-decision, but examines fewer nodes.  This
;;; version of alpha-beta works only for two players, and requires that the
;;; game is "zero-sum", i.e., the evaluation for one player is the opposite
;;; of the evaluation for the other.

(defun alpha-beta-decision (state game eval-fn &optional (limit 4))
  "Return the estimated best action, searching up to LIMIT and then
  applying the EVAL-FN."
  (car (the-biggest
        #'(lambda (a+s) 
            (first (right-rotate 
                    (alpha-value (cdr a+s) game
				 (game-worst game) (game-worst game)
                                 eval-fn (- limit 1)))))
        (game-successors state game))))

(defun alpha-value (state game alpha beta eval-fn limit)
  (cond ((game-over? game state) (terminal-values state))
	((= 0 limit) (funcall eval-fn state))
	(t (dolist (a+s (game-successors state game)
			(list alpha (- alpha)))
	     (setq alpha (max alpha
			      (first (right-rotate
				      (beta-value (cdr a+s) game alpha beta 
						  eval-fn (- limit 1))))))
	     (when (>= alpha (- beta))
	       (return (list (- beta) beta)))))))

(defun beta-value (state game alpha beta eval-fn limit)
  (cond ((game-over? game state) (terminal-values state))
	((= 0 limit) (funcall eval-fn state))
	(t (dolist (a+s (game-successors state game)
			(list beta (- beta)))
	     (setq beta (max beta
			     (first (right-rotate
				     (alpha-value (cdr a+s) game alpha beta 
						  eval-fn (- limit 1))))))
	     (when (>= beta (- alpha))
	       (return (list (- alpha) alpha)))))))
