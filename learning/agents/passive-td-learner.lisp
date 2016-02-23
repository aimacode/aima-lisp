;;; learning/agents/passive-td-learner.lisp
;;; Passive temporal-difference learning agent.
;;; After each transition, update the utility of the
;;; source state i to make it agree more closely with that
;;; of the destination state j.

(defvar *alpha* 1.0)     ;;; initial learning rate parameter

(defun make-passive-td-learner ()
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
	  (td-update U e percepts N)
	  (when (mdp-percept-terminalp e)
		(setq percepts nil)))
	'no-op)))

(defun td-update (U e percepts N &aux (terminalp (mdp-percept-terminalp e))
		                      (j (mdp-percept-state e))
		                      (r (mdp-percept-reward e)))
  (cond (terminalp
	 (setf (gethash j U)
	       (running-average (gethash j U) r (gethash j N))))
	((length>1 percepts)
	 (let* ((e2 (second percepts))
		(i (mdp-percept-state e2)))
	   (incf (gethash i U)
		 (* (current-alpha (gethash j N))
		    (+ r (- (gethash j U) (gethash i U)))))))))



(defun current-alpha (n)
  (/ (* 60 *alpha*)
     (+ 59 n)))
