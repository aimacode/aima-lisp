;;; learning/agents/active-adp-learner.lisp
;;; Reinforcement learning agent that uses dynamic
;;; programming to solve the Markov decision process
;;; that it learns from its experience. Thus, the
;;; main job is to update the model over time.

(defun make-random-adp-learner (actions)
  (make-active-adp-learner actions #'random-choice))
(defun make-maximizing-adp-learner (actions)
  (make-active-adp-learner actions #'max-choice))

(defun make-active-adp-learner (actions choice-function)
  (let* ((percepts nil)
	 (last-action nil)
	 (U (make-hash-table :test #'equal))
	 (N (make-hash-table :test #'equal))
	 (M (make-hash-table :test #'equal))
	 (R (make-hash-table :test #'equal))
	 (mdp (make-mdp :model M :rewards R)))
    #'(lambda (e)
	(push e percepts)
	(let ((s (mdp-percept-state e)))
	  (unless (gethash s N)               ;;; make entries for new state
		  (setf (gethash s N) 0
			(gethash s U) 0
			(gethash s M) (mapcar 
				       #'(lambda (a)
					   (cons a (make-mdp-action-model)))
				       actions)
			(gethash s R) (mdp-percept-reward e)))
	  (incf (gethash s N))
	  (update-active-model mdp percepts last-action)
	  (when (mdp-terminal-states mdp)     ;;; make sure DP alg. terminates
	    (setq U (value-iteration mdp U)))
	  (when (mdp-percept-terminalp e)
		(setq percepts nil))
	  (setq last-action (funcall choice-function s U M R))))))

;;; Update current model to reflect the evidence from the most recent action

(defun update-active-model (mdp        ;;; current description of envt.
			    percepts   ;;; in reverse chronological order
			    action     ;;; last action taken
			    &aux (M (mdp-model mdp)) transition)
  (when (length>1 percepts)
    (let* ((e (first percepts)) (j (mdp-percept-state (first percepts)))
	   (e2 (second percepts)) (i (mdp-percept-state e2))
	   (action-model (action-model action i M))
	   (transitions (mdp-action-model-transitions action-model)))
      (when (mdp-percept-terminalp e)
	(unless (member j (mdp-terminal-states mdp) :test #'equal)
	  (push j (mdp-terminal-states mdp))))
      (incf (mdp-action-model-times-executed action-model))
      (unless (setq transition 
		    (find j transitions :test #'equal 
			  :key #'transition-destination))
        (push (setq transition (make-transition :destination j))
	      (mdp-action-model-transitions action-model)))
      (incf (transition-times-achieved transition))
      (dolist (trans (mdp-action-model-transitions action-model))
	(setf (transition-probability trans) 
	      (float (/ (transition-times-achieved trans)
			(mdp-action-model-times-executed action-model))))))))
