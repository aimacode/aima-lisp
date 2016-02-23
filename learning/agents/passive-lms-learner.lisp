;;; learning/agents/passive-lms-learner.lisp
;;; Passive LMS learning agent.
;;; When a given training sequence terminates,
;;; update the utility of each state visited in the sequence
;;; to reflect the rewards received from then on.

(defun make-passive-lms-learner ()
  (let ((percepts nil)
	(U (make-hash-table :test #'equal))
	(N (make-hash-table :test #'equal)))
    #'(lambda (e)
	(push e percepts)
	(let ((s (mdp-percept-state e)))
	  (unless (gethash s N)  ;;; make entries for new state
		  (setf (gethash s N) 0
			(gethash s U) 0))
	  (incf (gethash s N))
	  (lms-update U e percepts N)
	  (when (mdp-percept-terminalp e)
		(setq percepts nil)))
	'no-op)))

(defun lms-update (U e percepts N &aux (reward-to-go 0))
  (when (mdp-percept-terminalp e)
    (dolist (ei percepts) ;;; percepts is in reverse chronological order
      (let ((i (mdp-percept-state ei))
	    (r (mdp-percept-reward ei)))
	(incf reward-to-go r)
	(setf (gethash i U)
	      (running-average (gethash i U) r (gethash i N)))))))
