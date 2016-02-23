;;; learning/agents/passive-adp-learner.lisp
;;; Reinforcement learning agent that uses dynamic
;;; programming to solve the Markov process
;;; that it learns from its experience. Thus, the
;;; main job is to update the model over time.
;;; Being a passive agent, it simply does no-op
;;; at each step, watching the world go by.

(defun make-passive-adp-learner ()
  (let ((percepts nil)
	(U (make-hash-table :test #'equal))
	(N (make-hash-table :test #'equal))
	(M (make-hash-table :test #'equal))
	(R (make-hash-table :test #'equal)))
    #'(lambda (e)
	(push e percepts)
	(let ((s (mdp-percept-state e)))
	  (unless (gethash s N)  ;;; make entries for new state
		  (setf (gethash s N) 0
			(gethash s U) 0
			(gethash s M) (list 
				       (cons 'no-op (make-mdp-action-model)))
			(gethash s R) (mdp-percept-reward e)))
	  (incf (gethash s N))
	  (update-passive-model s percepts M)
	  (setq U (value-determination (passive-policy M) U M R))
	  (when (mdp-percept-terminalp e)
		(setq percepts nil)))
	'no-op)))

;;; Updating the transition model according to oberved transition i->j.
;;; Fairly tedious because of initializing new transition records.

(defun update-passive-model 
  (j          ;;; current state (destination of transition)
   percepts   ;;; in reverse chronological order
   M          ;;; transition model, indexed by state
   &aux transition)
  (when (length>1 percepts)
    (let* ((e2 (second percepts))
	   (i (mdp-percept-state e2)) ;;; transition from i, so update i's model
	   (action-model (action-model 'no-op i M))
	   (transitions (mdp-action-model-transitions action-model)))
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

;;; (passive-policy M) makes a policy of no-ops for use in value determination

(defun passive-policy (M)
  (copy-hash-table M #'(lambda (x) (declare (ignore x)) 
			 (list (list 'no-op 1.0)))))