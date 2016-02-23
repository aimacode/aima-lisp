;; -*- Mode: Lisp; -*- 

;;;; Problem-Solving Agents

(defstructure (problem-solving-agent (:include agent))
  "An agent that applies a search algorithm to a problem to find a solution,
  and then follows the steps of the solution, one at a time, and then stops."
  (algorithm 'A*-search))

;;;; Game-Playing Agents

(defstructure (game-agent (:include agent))
  "An agent that plays n-player games.  The ALGORITHM slot is filled by
  a function that takes state and game arguments, and returns a move."
  (algorithm 'minimax-decision))

(defstructure (random-game-agent 
	       (:include game-agent (algorithm 'pick-random-move)))
  "A game-playing agent that picks randomly from the legal moves.")

(defstructure (human-game-agent 
	       (:include game-agent (algorithm 'ask-game-user)))
  "A game-playing agent that asks the human user what to do.")

(defun pick-random-move (state game)
  (random-element (or (legal-moves game state) '(nothing))))

(defun ask-game-user (state game)
  (let ((legal-moves (legal-moves game state)))
    (loop (format t "~&~A's move? " (current-player state))
      (let ((move (read)))
	(when (member move legal-moves :test #'equal)
	  (RETURN move))
	(format t "~&~A is illegal for ~A.  Choose one of:~%  ~A.~%"
		move (current-player state) legal-moves)))))
