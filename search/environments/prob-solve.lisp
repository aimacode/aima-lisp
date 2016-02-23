;;; -*- Mode: Lisp; -*- Author: Peter Norvig

;;;; Problem-Solving Environments

;;; The basic problem-solving-environment type, and the main generic
;;; functions for it.

(defstructure (problem-solving-environment (:include environment))
  "An environment in which to solve problems.  The state of the environment
  is one of the states from the problem, starting with the initial state."
  (problem (required)))

(defmethod get-percept ((env problem-solving-environment) agent)
  "All agents can access the complete state of the environment."
  (declare-ignore agent)
  (environment-state env))

(defmethod update-fn ((env problem-solving-environment))
  "Set the state to the result of executing the agent's action."
  (setf (environment-state env)
	(cdr (assoc (agent-action (first (environment-agents env)))
		    (successors (problem-solving-environment-problem env) 
				(environment-state env))
		    :test #'equal))))

(defmethod performance-measure ((env problem-solving-environment) agent)
  "Score of 1 for solving problem; 0 otherwise."
  (declare (ignore agent))
  (if (termination? env) 1 0))

(defmethod initialize ((env problem-solving-environment))
  "Get the initial state from the problem, and supply agents with programs."
  (let ((problem (problem-solving-environment-problem env)))
    (setf (environment-state env) (problem-initial-state problem))
    ;; A problem solving agent needs to know what problem to solve.
    ;; We take the problem from the environment and use it to create
    ;; a problem-solving agent program for each agent (we only expect one).
    (for each agent in (environment-agents env) do
	 (setf (agent-program agent) (problem-solving-program 
				      (problem-solving-agent-algorithm agent)
				      problem)))))

(defun problem-solving-program (search-algorithm problem)
  "Given a search algorithm, return a program that at the start searches
  for a solution, then executes the steps of the solution, then stops."
  (let ((actions :start))
    #'(lambda (percept) 
	(declare (ignore percept)) ;; These agents ignore percepts!
	(when (eq actions :start)
	  (setf actions (solution-actions (funcall search-algorithm problem))))
	(if actions (pop actions) :stop))))

(defmethod termination? ((env problem-solving-environment))
  "Stop when the problem is solved, or when an agent says stop."
  (or (goal-test (problem-solving-environment-problem env) 
		 (environment-state env))
      (find :stop (environment-agents env) :key #'agent-action)))

;;;; Converting a Problem to an Environment

(defun problem->environment (problem &key (algorithm 'A*-search))
  "Convert a problem into an environment.  Then we can pass the environment
  to RUN-ENVIRONMENT, and the agent will search for a solution and execute it."
  (make-problem-solving-environment 
   :agents (list (make-problem-solving-agent :algorithm algorithm))
   :problem problem))

(defmethod print-structure ((env problem-solving-environment) stream)
  (format stream "#<~A; State: ~A>" (type-of env) (environment-state env)))