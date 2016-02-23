;;; learning/agents/exploring-adp-learner.lisp
;;; Reinforcement learning agent that uses dynamic
;;; programming to solve the Markov decision process
;;; that it learns from its experience. Thus, the
;;; main job is to update the model over time.
;;; Unlike the active-adp-learner, this agent uses
;;; an "intelligent" exploration policy to make sure it
;;; explores the state space reasonably quickly.

(defvar *R+* 2)
(defvar *Ne* 5)

(defun exploration-function (u n)
  (if (< n *Ne*) *R+* u))

(defun make-exploring-adp-learner (actions)
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
	  (when (mdp-terminal-states mdp)
	    (setq U (exploratory-value-iteration mdp U)))
	  (when (mdp-percept-terminalp e)
		(setq percepts nil))
	  (setq last-action (exploration-choice s U M R))))))


;;; Given an environment model M, determine the values of states U.
;;; Use value iteration, with initial values given by U itself.
;;; Basic equation is U(i) <- r(i) + max_a f(sum_j  M(a,i,j)U(j), N(a,i))
;;; where f is the exploration function. Does not applyt to terminal states.

(defun exploratory-value-iteration 
  (mdp &optional (Uold (copy-hash-table (mdp-rewards mdp) #'identity))
       &key (epsilon 0.000001)
       &aux (Unew (copy-hash-table Uold #'identity))
            (max-delta infinity) 
	    (M (mdp-model mdp))
	    (R (mdp-rewards mdp)))
  (do ()
      ((< max-delta epsilon) Unew)
    (setq max-delta 0)
    (rotatef Uold Unew) ;;; switch contents; then we will overwrite Unew
    (maphash 
     #'(lambda (s u) 
	 (unless (sink? s M)
	   (setf (gethash s Unew)
		 (+ (gethash s R)
		    (if (gethash s M)
			(apply #'max
			       (mapcar
				#'(lambda (a) 
				    (if (member s (mdp-terminal-states mdp)
						:test #'equal)
					(gethash s R)
				      (exploration-function
				       (q-value a s Uold M R)
				       (mdp-action-model-times-executed
					(action-model a s M)))))
				(actions s M)))
		      0))))
	 (setq max-delta (max max-delta (abs (- (gethash s Unew) u)))))
     Uold)))


(defun exploration-choice (s U M R)
  (the-biggest-random-tie 
   #'(lambda (a)
       (exploration-function 
	(q-value a s U M R)
	(mdp-action-model-times-executed (action-model a s M))))
   (actions s M)))

