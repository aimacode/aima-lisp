;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Definitions for Markov Decision Problems and Reinforcement Learning

(defstructure (mdp-environment (:include environment))
  "An MDP-environment is driven by an MDP (Markov Decision Process),
  which (probabilistically) says what state to transition to for each action."
  ;;; To make an MDP into an environment, we basically just keep track of the
  ;;; current state, and then ask the MDP model to determine the new state.
  ;;; This makes sense for the case of a single agent in the environment.
  (mdp (make-mdp))
  (epochs-left 1))

(defstruct (mdp-percept (:type list))
  "A percept gives the current state, the reward received, and whether it is
  a terminal state."
  state reward terminalp)

;;;; Generic Functions for MDP-Environments

(defmethod initialize ((env mdp-environment))
  ;; Set the initial state, and make sure there is one agent.
  (setf (environment-state env) (mdp-initial-state (mdp-environment-mdp env)))
  (call-next-method)
  (assert (= 1 (length (environment-agents env)))))

(defmethod get-percept ((env mdp-environment) agent)
  "The percept is the current state, the reward, and whether this is terminal."
  (declare (ignore agent))
  (let* ((mdp (mdp-environment-mdp env))
	 (state (environment-state env))
	 (state-key (funcall (mdp-hash-key mdp) state)))
    (make-mdp-percept
     :state state
     :reward (gethash state-key (mdp-rewards mdp))
     :terminalp (not (null 
		      (member state (mdp-terminal-states mdp) 
			      :test #'equal))))))

(defmethod update-fn ((env mdp-environment))
  "We update by transitioning to a new state.  When we hit a terminal state,
  we restart in the initial state (until there are no more epochs left)."
  (let ((mdp (mdp-environment-mdp env))
	(agent (first (environment-agents env))))
    (incf (mdp-agent-total-reward agent) 
	  (mdp-percept-reward (agent-percept agent)))
    (cond ((member (environment-state env) (mdp-terminal-states mdp) 
		   :test #'equal)
	   ;; Start over when we reach a terminal state
	   (decf (mdp-environment-epochs-left env))
	   (setf (environment-state env) (mdp-initial-state mdp)))
	  (t (setf (environment-state env)
		   (mdp-next-state (agent-action agent)
				   (environment-state env)
				   mdp))))))

(defmethod performance-measure ((env mdp-environment) agent)
  "Return a number saying how well this agent is doing."
  ;; The default is to subtract one point for each time step.
  (mdp-agent-total-reward agent))

(defmethod termination? ((env mdp-environment))
  (= 0 (mdp-environment-epochs-left env)))

;;;; Utility Functions

(defun mdp-next-state (action state mdp)
  (let ((state-key (funcall (mdp-hash-key mdp) state)))
    (random-transition 
     (mdp-transitions action (gethash state-key (mdp-model mdp))))))

(defun mdp-transitions (action state-model)
  (mdp-action-model-transitions 
   (cdr (assoc action state-model :test #'equal))))
                       
(defun random-transition (transitions &aux (r (random 1.0)))
  (dolist (transition transitions)
    (decf r (transition-probability transition))
    (unless (plusp r) (return (transition-destination transition)))))

