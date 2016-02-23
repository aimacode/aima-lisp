;;;; Code for performance assessment of DP and RL algorithms.

;;; Makes extensive use of global variables to minimize interference with the
;;; algorithms themselves.

(defvar *policy-fn*)   ;;; the policy used by the agent in acting
(defvar *correct-U*)
(defvar *correct-M*)
(defvar *correct-R*)

;;;; U2 is the correct utility table
;;;; assume U1, U2 have the same states
(defun u-rms-error (U1 U2 &aux (n 0) (e 0)) 
  (maphash #'(lambda (s u)
	       (incf n)
	       (incf e (square (- u (gethash s U2)))))
	   U1)
  (sqrt (/ e n)))

;;; The policy loss of a utility function U for an mdp is defined as the
;;; difference in utility between the corresponding policy and the optimal
;;; policy, for the agent's current state. Calculate using
;;; value determination wrt the current policy 

(defun loss (mdp U &aux (U2 (copy-hash-table U #'identity))
		        (M (mdp-model mdp))
			(R (mdp-rewards mdp)))
  (maphash #'(lambda (s md) (declare (ignore md))
	       (unless (gethash s U2) (setf (gethash s U2) 0)))
	   *correct-R*)       ;;; fill in missing entries if any
  (setq U2 (value-determination (funcall *policy-fn* U M R) 
				U2 *correct-M* *correct-R*))
  (- (gethash (mdp-initial-state mdp) *correct-U*)
     (gethash (mdp-initial-state mdp) U2)))

