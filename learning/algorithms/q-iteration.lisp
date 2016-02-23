;;; learning/algorithms/q-iteration.lisp
;;; Data structures and algorithms for calculating the Q-table for an
;;; MDP. Q(a,i) is the value of doing action a in state i.

(defun q-entry (Q a i) (cdr (assoc a (gethash i Q) :test #'eq)))

(defun all-q-entries (i Q) (mapcar #'cdr (gethash i Q)))

(defun q-actions (s Q) (mapcar #'car (gethash s Q)))

;;; Given an MDP, determine the q-values of the states.
;;; Q-iteration iterates on the Q-values instead of the U-values.
;;; Basic equation is Q(a,i) <- R(i) + sum_j M(a,i,j) max_a' Q(a',j)
;;; where Q(a',j) MUST be the old value not the new.
(defun q-iteration (mdp &optional (Qold nil)
			&key (epsilon 0.000001)
			&aux Qnew
			     (max-delta infinity) 
			     (M (mdp-model mdp))
			     (R (mdp-rewards mdp)))
  (unless Qold
    (setq Qold (make-hash-table :test #'equal))
    (maphash #'(lambda (s r)
		 (setf (gethash s Qold)
		       (mapcar #'(lambda (a) (cons a r)) (actions s M))))
	     R))
  (setq Qnew (copy-hash-table Qold #'identity))
  (do ()
      ((< max-delta epsilon) Qnew)
    (setq max-delta 0)
    (maphash #'(lambda (s a-qs) 
		 (dolist (a-q a-qs) (setf (cdr a-q) 
					  (q-entry Qnew (car a-q) s))))
	     Qold)
    (maphash 
     #'(lambda (i a-qs)
	 (let ((new-a-qs (gethash i Qnew)))
	   (dolist (a-q a-qs)
	     (let ((a (car a-q)) (q (cdr a-q)))
	       (unless (sink? i M)
		 (setf (cdr (assoc a new-a-qs :test #'eq))
		       (+ (gethash i R)
			  (average-successor-q a i Qold M))))
	       (setq max-delta (max max-delta 
				(abs (- (q-entry Qnew a i) q))))))))
     Qold)))

(defun average-successor-q (a i Q M &aux (sum 0))
  (dolist (transition (transitions a i M) sum)
    (let* ((j (transition-destination transition)) 
	   (p (transition-probability transition)) 
	   (qjs (gethash j Q)))
      (incf sum 
	    (if qjs
		(* p (cdr (the-biggest #'cdr qjs)))
	      0)))))




;;; Compute optimal policy from Q table
(defun q-optimal-policy (Q &aux (P (make-hash-table :test #'equal)))
  (maphash #'(lambda (s a-qs) (declare (ignore a-qs))
	       (setf (gethash s P) 
		     (list (list (q-dmax-choice s Q) 1.0))))
	   Q)
  P)


;;; Choice functions select an action under specific circumstances

;;; Pick a random action
(defun q-random-choice (s Q)
  (random-element (q-actions s Q)))

;;; Pick the currently best action
(defun q-dmax-choice (s Q)
  (car (the-biggest #'cdr (gethash s Q))))

;;; Pick the currently best action with tie-breaking
(defun q-max-choice (s Q)
  (car (the-biggest-random-tie #'cdr (gethash s Q))))


