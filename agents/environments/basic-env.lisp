;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The basic environment simulator code

;;; This file defines the environment simulator function: RUN-ENVIRONMENT.  It
;;; is a little different from the pseudo-code in the book: to make it easier
;;; to write new environments, we used an object-oriented approach. Rather
;;; than requiring the caller to pass in a bunch of functions to
;;; RUN-ENVIRONMENT, you will define methods for the functions for each
;;; subtype of environment.  We added a DISPLAY parameter to control whether
;;; intermediate results are displayed. As an example, the following
;;; expression builds an environment and runs an agent in it, displaying the
;;; results:
;;;
;;; (run-environment (make-vacuum-world :aspec '(random-vacuum-agent)))
;;;
;;; We also define AGENT-TRIALS to compare the performance of several
;;; different agents in a series of random environments, all drawn from an
;;; environment subtype.  For example to run 2 agents in 20 environments each:
;;;
;;; (agent-trials 'vacuum-world
;;;   '(random-vacuum-agent reactive-vacuum-agent) :n 20)

(defstructure environment
  "The world in which agents exist."
  (agents '())       ;; A list of the agents in the environment
  (step 0)           ;; The number of time steps simulated so far
  (max-steps 1000)   ;; Stop the simulation after this number
  (stream t)         ;; Stream to display output on
  (initialized nil)  ;; Have we run initialize on this environment yet?
  (state nil)        ;; Current state of the environment; other subtypes
                     ;; add new slots to hold various state information
  )

;;; An agent is something that perceives and acts.  As such, each agent has a
;;; slot to hold its current percept, and its current action.  The action
;;; will be handed back to the environment simulator to perform (if legal).
;;; Each agent also has a slot for the agent program, and one for its score
;;; as determined by the performance measure.

(defstructure agent
  "Agents take actions (based on percepts and the agent program) and receive
  a score (based on the performance measure).  An agent has a body which can
  take action, and a program to choose the actions, based on percepts."
  (program #'nothing)			; fn: percept -> action
  (body (make-agent-body))
  (score 0)
  (percept nil)
  (action nil)
  (name nil))

;;;; Top level functions

(defun run-environment (env)
  "Basic environment simulator.  It gives each agent its percept, gets an
  action from each agent, and updates the environment. It also keeps score
  for each agent, and optionally displays intermediate results. [p 48]"
  (initialize env)
  (display-environment env)
  (dotimes (i (environment-max-steps env))
    (incf (environment-step env))
    ;; Deliver percept and get action from each agent
    (for each agent in (environment-agents env) do
	 (setf (agent-percept agent) (get-percept env agent))
	 (setf (agent-action agent) 
	       (funcall (agent-program agent) (agent-percept agent))))
    ;; Execute the actions and otherwise update the world
    (update-fn env)
    ;; Update the agent scores, then optionally display the current state
    (for each agent in (environment-agents env) do
	 (setf (agent-score agent) (performance-measure env agent)))
    (display-environment env)
    (when (termination? env) (RETURN)))
  env)

(defun agent-trials (environment-fn agent-types &key (n 10))
  "Report how well a single agent does in a set of N similar environments,
  and compare that to other agents in the same set of environments.
  Environment-fn takes a :agents keyword argument, and returns an environment.
  Agent-types is a list of names of functions that each create an agent."
  (let ((env-gen-random-state (make-random-state t)))
    (mapcar #'(lambda (agent-type)
		(agent-trial environment-fn agent-type
			     (make-random-state env-gen-random-state) n))
	    agent-types)))

;;;; Generic Functions that must be defined for each environment

;;; For each new type of environment you want to define, you will need a
;;; defstructure that inherits from (includes) ENVIRONMENT, and you will need
;;; to write new methods (or inherit existing methods) for each of the
;;; following eight functions.  Here are the ones that will change for each
;;; new environment:

(defmethod get-percept ((env environment) agent)
  "Return the percept for this agent."
  (declare-ignore env agent)
  nil)

(defmethod update-fn ((env environment))
  "Modify the environment, based on agents actions, etc."
  (execute-agent-actions env))

(defmethod legal-actions ((env environment))
  "A list of the action operators that an agent can do."
  nil)

(defmethod performance-measure ((env environment) agent)
  "Return a number saying how well this agent is doing."
  ;; The default is to subtract one point for each time step.
  (declare-ignore agent)
  (- (environment-step env)))

;;; Here are the ones that can usually be inherited:

(defmethod initialize ((env environment))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (initialize-agent-names env)
  (setf (environment-initialized env) t)
  env)

(defmethod termination? ((env environment))
  "Return true if the simulation should end now."
  nil)

(defmethod display-environment ((env environment))
  "Display the current state of the environment."
  ;; You probably won't need to specialize this, unless you want to do
  ;; a fancy graphical user interface
  (let ((stream (environment-stream env)))
    (when stream 
      (format stream "~&At Time step ~D:~%" (environment-step env))
      (when (> (environment-step env) 0)
	(for each agent in (environment-agents env) do
	     (format stream 
		     "~&Agent ~A perceives ~A~%~6Tand does ~A~%"
		     agent (agent-percept agent)
		     (agent-action agent))))
      (display-environment-snapshot env))))

(defmethod display-environment-snapshot ((env environment))
  "Display a 'picture' of the current state of the environment."
  ;; This is what you will specialize 
  (print env (environment-stream env)))

;;;; Auxiliary Functions

(defun run-eval-environment (env)
  "Basic environment simulator; the same as run-environment. [p 48]
  We decided it was silly to run the environment and NOT eval the agents,
  so in this code, and in future editions of the book, we will only have
  RUN-ENVIRONMENT, and it will evaluate the agents."
  (run-environment env))

(defun agent-trial (environment-fn agent-type env-gen-random-state n)
  "Run n environments with an identical agent in each, and average the scores."
  ;; By binding *random-state* to env-gen-random-state, we hope to reproduce
  ;; the same set of environments each time AGENT-TRIAL is called with the
  ;; same environment-fn.
  (let ((total 0) (score 0))
    (for i = 1 to n do
	 (let* ((env (let ((*random-state* env-gen-random-state))
		       (funcall environment-fn 
				:stream nil
				:aspec (list agent-type)))))
	   (run-environment env)
	   (incf total (agent-score (first (environment-agents env))))))
    (setf score (float (/ total n)))
    (format t "~&~10,2F average for ~A" score agent-type)
    score))

(defun execute-agent-actions (env)
  "Each agent (if the agent is alive and has specified a legal action)
  takes the action."
  (for each agent in (environment-agents env) do
       (let ((act (agent-action agent)))
	 (when (member (op act) (legal-actions env))
	   (apply (op act) env (agent-body agent) (args act))))))

(defmethod print-structure ((env environment) stream)
  (format stream "#<~A; Step: ~D, Agents:~{ ~A~}>"
	  (type-of env) (environment-step env)
	  (environment-agents env)))





