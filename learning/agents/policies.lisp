;;; learning/agents/policies.lisp
;;; Definitions for policies and choice functions used by reinforcement
;;; learning agents for exploration etc.

;;; Compute wacky policy table - a randomized policy uniformly distributed over possible actions
(defun wacky-policy (U M R &aux (P (make-hash-table :test #'equal))) (declare (ignore U R))
  (maphash #'(lambda (s md)
	       (setf (gethash s P) 
		     (mapcar #'(lambda (ants) (list (car ants)
						    (/ 1.0 (length md))))
			     md)))
	   M)
  P)


