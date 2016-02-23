;;; learning/agents/active-adp-learner.lisp
;;; Reinforcement learning agent that uses dynamic
;;; programming to solve the Markov decision process
;;; that it learns from its experience. Thus, the
;;; main job is to update the model over time.

(defun make-random-qi-learner (actions)
  (make-active-qi-learner actions #'q-random-choice))
(defun make-maximizing-qi-learner (actions)
  (make-active-qi-learner actions #'q-max-choice))

(defun make-active-qi-learner (actions choice-function)
  (let* ((percepts nil)
	 (last-action nil)
	 (Q (make-hash-table :test #'equal))
	 (N (make-hash-table :test #'equal))
	 (M (make-hash-table :test #'equal))
	 (R (make-hash-table :test #'equal))
	 (mdp (make-mdp :model M :rewards R)))
    #'(lambda (e)
	(push e percepts)
	(let ((s (mdp-percept-state e)))
	  (unless (gethash s N)               ;;; make entries for new state
  	    (setf (gethash s N) 0
		  (gethash s Q) (mapcar #'(lambda (a)
					    (cons a (mdp-percept-reward e)))
					actions)
		  (gethash s M) (mapcar #'(lambda (a)
					    (cons a (make-mdp-action-model)))
					actions)
			(gethash s R) (mdp-percept-reward e)))
	  (incf (gethash s N))
	  (update-active-model mdp percepts last-action)
	  (when (mdp-terminal-states mdp)     ;;; make sure DP alg. terminates
	    (setq Q (q-iteration mdp Q)))
	  (when (mdp-percept-terminalp e)
		(setq percepts nil))
	  (setq last-action (funcall choice-function s Q))))))

