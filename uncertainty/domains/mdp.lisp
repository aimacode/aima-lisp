;;; Definitions for Markov decision processes (MDPs).

;;; An MDP is defined by initial state, transition model, rewards, and
;;; distinguished terminal states. Model and rewards are hash tables
;;; index by state (after application of hash-key function).
;;; The entries in the model are alists keyed by action; each action is
;;; associated with an action model: basically a list of transitions.
;;; Markov chains (i.e., stochastic processes with no distinct agent)
;;; can be defined by allowing only a no-op action in the MDP.

(defstruct mdp
  initial-state               ;; The initial state for the problem
  (model (make-hash-table :test #'equal)) ;; Describes transition probabilities
  (rewards (make-hash-table :test #'equal)) ;; Rewards for each state
  (terminal-states nil)       ;; List of terminal states
  (hash-key #'identity)       ;; To convert states into hash keys
  name)                       ;; String, identifies the problem

(defstruct (mdp-action-model (:type list))
  (transitions nil)
  (times-executed 0))

(defstruct (transition (:type list))
  destination
  probability
  (times-achieved 0))
  
(defun action-model (a s M)
  (cdr (assoc a (gethash s M) :test #'eq)))

(defun transitions (a s M) 
  "Returns the transitions resulting from executing
  action a in state s according to model M."
  (mdp-action-model-transitions (action-model a s M)))

(defun actions (s M) 
  "Returns the list of actions feasible in state s according to model M."
  (mapcar #'car (gethash s M)))















