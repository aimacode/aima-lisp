;;;; Basic dynamic programming routines for MDPs (Markov decision processes)

;;; Value iteration, value determination, and policy iteration. 
;;; MDP agents pass in an mdp and expect a policy in return.

(defun value-iteration-policy (mdp)
  (optimal-policy (value-iteration mdp) (mdp-model mdp) (mdp-rewards mdp)))

;;; Given an environment model M, value iteration
;;; determine the values of states U.
;;; Basic equation is U(i) <- r(i) + max_a sum_j  M(a,i,j)U(j)
;;; where U(j) MUST be the old value not the new.

(defun value-iteration (mdp &optional (Uold (copy-hash-table
					     (mdp-rewards mdp) #'identity))
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
				#'(lambda (a) (q-value a s Uold M R))
				(actions s M)))
		      0))))
	 (setq max-delta (max max-delta (abs (- (gethash s Unew) u)))))
     Uold)))

;;; A state is a sink if there are no actions that can lead to another state.
;;; Sinks can arise by accident during reinforcement learning of an environment
;;; model. Because they cause infinite loops, they must be detected.
(defun sink? (s M)
  (not (some #'(lambda (a)
		 (some #'(lambda (transition) 
			   (not (equal s (transition-destination transition))))
		       (transitions a s M)))
	     (actions s M))))


;;; Given an initial policy P and initial utilities U, calculate the optimal
;;; policy. Do this by value determination alternating with policy update.

(defun policy-iteration (mdp &optional (U (copy-hash-table
					   (mdp-rewards mdp) #'identity))
			     &aux (M (mdp-model mdp))
				  (R (mdp-rewards mdp))
				  (P (optimal-policy U M R))
			          (unchanged nil) new)
  (do ()
      (unchanged P)
    (setq unchanged t)
    (setq U (value-determination P U M R))
    (maphash #'(lambda (s aplist) (declare (ignore aplist))
		 (setq new (dmax-choice s U M R))
		 (when (> (q-value new s U M R) 
			  (q-value (caar (gethash s P)) s U M R) )
		       (setq unchanged nil)
		       (setf (gethash s P) (list (list new 1.0)))))
	     P)))

;;; Given a fixed policy and a model, calculate the value of each state.
;;; This version does it by an iterative process similar to value iteration.
;;; Basic equation is U(i) <- r(i) + sum_j  M(P(i),i,j)U(j)
;;; where U(j) MUST be the old value not the new.
;;; A better alternative is to set up the value equations and solve them
;;; using matrix methods.

(defun value-determination (P Uold M R
			    &key (epsilon 0.000001)
			    &aux Unew
			         (max-delta infinity))
  (setf Unew (copy-hash-table Uold #'identity))
  (do ()
      ((< max-delta epsilon) Unew)
    (setq max-delta 0)
    (rotatef Uold Unew)
    (maphash 
     #'(lambda (s u) 
	 (unless (sink? s M)
	   (setf (gethash s Unew)
		 (+ (gethash s R)
		    (if (gethash s M)
			(q-value (caar (gethash s P)) s Uold M R)
		      0))))
	 (setq max-delta (max max-delta (abs (- (gethash s Unew) u)))))
     Uold)))


;;; Compute optimal policy given U and M
(defun optimal-policy (U M R &aux (P (make-hash-table :test #'equal)))
  (maphash #'(lambda (s md) (declare (ignore md))
	       (setf (gethash s P) (list (list (max-choice s U M R) 1.0))))
	   M)
  P)


;;; The following functions select actions in particular states

;;; Pick a random action

(defun policy-choice (state P &aux (aplist (gethash state P))
				   (r (random 1.0)))
  (dolist (a-p aplist)
    (decf r (second a-p))
    (unless (plusp r) (return (first a-p)))))
  

(defun random-choice (state U M R) (declare (ignore state U M R))
  (random-element '(left right up down)))

;;; Pick the currently best action with tie-breaking
(defun max-choice (state U M R)
  (car (the-biggest-random-tie 
	#'(lambda (ants) (q-value (car ants) state U M R))
	(gethash state M))))

;;; Simply pick a currently best action deterministically
(defun dmax-choice (state U M R)
  (car (the-biggest
	#'(lambda (ants) (q-value (car ants) state U M R))
	(gethash state M))))


;;; Q(a,s) is the value of doing a in s, calculated by averaging over the
;;; utilities of the possible outcomes. Used in several update equations.

(defun q-value (action state U M R &aux (v 0))
  (declare (ignore R))
  (dolist (transition (transitions action state M) v)
    (incf v (* (transition-probability transition)
	       (gethash (transition-destination transition) U)))))


