;;; -*- Mode: Lisp; -*-

;;;; Game-Playing

;;; Definitions for all n-player turn-taking games.  The idea is that
;;; each particular game we want to deal with will define a subtype of
;;; the GAME structure, with methods for LEGAL-MOVES, MAKE-MOVE, and
;;; GAME-OVER?.
;;;
;;; In the description of a game, a player is an atomic name: X or O,
;;; or BLACK or WHITE.  The current state is kept as an instance of
;;; GAME-STATE (which we do not expect to create subtypes of).

;;; We use GAME->ENVIRONMENT to turn an instance of a game into an
;;; environment, which we can then run.  The function RUN-GAME
;;; provides a simple interface to do this. Corresponding to player X
;;; there is an agent with name X who has an agent program that
;;; chooses a move for X, given a game-state as the percept.

(defstructure game 
  "A game is defined in terms of the starting position (the initial-state),
  the rewards for winning and losing, and three generic functions:
  LEGAL-MOVES: a list of moves that can be made in this state
  MAKE-MOVE: generate a new state
  GAME-OVER?: are we finished?
  You provide methods for these for each new subtype of game."
  initial-state				; a state in the domain
  (best +1)				; Best score (for winning the game)
  (worst -1)				; Worst score (for losing the game)
  )

(defstructure game-state 
  "Everything you need to know about the state of the game. The players
  are given as a list of names, with the player whose move it is
  first.  A property list keeps the scores for each player.  In some
  games, scores are accumulated as you go, in others a score is
  awarded only at the end."
  board               ; the current state of the game board
  (players '(X O))    ; list of the players, current player to move first
  (scores '(X 0 O 0)) ; Plist of scores for each player
  (terminal? nil)     ; Does this state end the game?
  (previous-move nil) ; The move just made to get to this position
  )

;;;; Generic Functions for Games

(defmethod legal-moves ((game game) state)
  "Return a list of legal moves"
  (required "You need to provide a LEGAL-MOVES method for ~A" game state))

(defmethod make-move ((game game) state move)
  "Return the new state that results from making this move."
  (required "You need to provide a MAKE-MOVE method for ~A" game state move))

(defmethod game-over? ((game game) state)
  "Is the game finished?"
  (required "You need to provide a GAME-OVER? method for ~A" game state))

;;;; Game-playing environments

(defstructure (game-environment (:include environment))
  (game (required)))

(defun game->environment (game &key agents)
  "Convert a game into an environment.
  AGENTS can be a list of agents or agent types."
  (make-game-environment :game game :agents agents))

(defun run-game (game &key agents)
  "Build an environment around this game, and run it."
  (run-environment (game->environment game :agents agents)))

;;;; Implementation of Generic Functions for Game Environments

(defmethod update-fn ((env game-environment))
  (let ((action (agent-action (current-agent env)))
	(state (environment-state env))
	(game (game-environment-game env)))
    ;; Only allow legal moves
    (when (legal-move? action state game)
      (let ((new-state (make-move game state action)))
	;; Copy the scores into the agent structures
	(assign-agent-scores state env)
	;; Update the environment
	(setf (environment-state env) new-state)))))

(defmethod performance-measure ((env game-environment) agent)
  "Look up the agent's score in the current state."
  (getf (game-state-scores (environment-state env)) (agent-name agent) 0))

(defmethod get-percept ((env game-environment) agent)
  "We assume all agents can perceive the whole state."
  (declare-ignore agent)
  (environment-state env))

(defmethod initialize ((env game-environment))
  "Install an agent program (based on the agent's algorithm slot) in each 
  agent.   The program makes sure an agent only moves when it is its turn.   
  Also initialize the name of each agent, and the environment's state."
  (setf (environment-state env) 
	(game-initial-state (game-environment-game env)))
  (setf (environment-agents env)
	(mapcar #'(lambda (agent name)
		    (if (symbolp agent) (setf agent (make agent)))
		    (setf (agent-name agent) name)
		    (setf (agent-body-name (agent-body agent)) name)
		    (setf (agent-program agent)
			  #'(lambda (state)
			      (if (eq agent (current-agent env))
				  (funcall (game-agent-algorithm agent)
					   state
					   (game-environment-game env))
				'nothing)))
		    agent)
		(append (environment-agents env) 
                        (make-list 
                         (length (game-state-players (environment-state env)))
                         :initial-element 'random-game-agent))
		(game-state-players (environment-state env)))))

(defmethod termination? ((env game-environment))
  (let* ((state (environment-state env))
	 (done (game-over? (game-environment-game env) state)))
    (assign-agent-scores state env)
    done))


(defun assign-agent-scores (state env)
  (mapc #'(lambda (player)
	    (setf (agent-score (agent-with-name player env))
		  (getf (game-state-scores state) player 0)))
	(game-state-players state)))

;;;; Auxiliary functions

(defun legal-move? (move state game)
  (member move (legal-moves game state) :test #'equal))

(defun current-player (game-state)
  (first (game-state-players game-state)))

(defun previous-player (game-state)
  (last1 (game-state-players game-state)))

(defun game-players (game)
  (game-state-players (game-initial-state game)))

(defun current-agent (env)
  (agent-with-name (current-player (environment-state env)) env))

(defun agent-with-name (name env)
  (find name (environment-agents env) :key #'agent-name :test #'equalp))

(defmethod print-structure ((game game) stream)
  (format stream "#<~A state: ~A>"
	  (type-of game) (game-initial-state game)))

(defmethod print-structure ((state game-state) stream) 
  (format stream "#<~A>" (type-of state)))

(defmethod display-environment ((env game-environment)) 
  (let ((stream (environment-stream env))
	(state (environment-state env)))
    (when stream
      (when (game-state-previous-move state)
	(format stream "~&~A moved to ~A.~%" (previous-player state)
		(game-state-previous-move state)))
      (print-grid (game-state-board state) :stream stream)
      (format stream "~&~A to move:" (current-player state)))))
