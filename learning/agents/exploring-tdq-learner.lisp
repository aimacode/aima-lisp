;;; learning/agents/exploring-tdq-learner.lisp
;;; Exploratory reinforcement learning agent using temporal differences.
;;; Works without a model by using the stochastic sampling to
;;; mirror the effect of averaging using the model.

(defun make-exploring-tdq-learner (actions)
  (let ((i nil)           ;;; the previous state visited
	(a nil)           ;;; the last action taken
	(Q (make-hash-table :test #'equal))
	(N (make-hash-table :test #'equal))
	(Ri nil))         ;;; reward received in state i
    #'(lambda (e)
	(let ((terminalp (mdp-percept-terminalp e))
	      (j (mdp-percept-state e))
	      (reward (mdp-percept-reward e)))
	  (unless (gethash j Q)
	    (setf (gethash j Q) 
		  (mapcar #'(lambda (a) (cons a reward)) actions))
	    (setf (gethash j N) 
		  (mapcar #'(lambda (a) (cons a 0)) actions)))
	  (when i
	    (incf (cdr (assoc a (gethash i N) :test #'eq)))
	    (update-exploratory-Q Q a i j N Ri))
	  (cond (terminalp 
		 (setq i nil)
		 (setf (gethash j Q) 
		       (mapcar #'(lambda (a) (cons a reward)) actions)))
		(t (setq i j Ri reward)))
	  (setq a (exploration-q-choice j Q N))))))


(defun update-exploratory-Q (Q a i j N Ri)
  (incf (cdr (assoc a (gethash i Q) :test #'eq))
	(* (current-alpha (cdr (assoc a (gethash i N) :test #'eq)))
	   (+ Ri
	      (- (apply #'max (all-q-entries j Q))
		 (q-entry Q a i))))))

(defun exploration-q-choice (s Q N)
  (the-biggest-random-tie 
   #'(lambda (a)
       (exploration-function 
	(q-entry Q a s)
	(cdr (assoc a (gethash s N) :test #'equal))))
   (q-actions s Q)))

