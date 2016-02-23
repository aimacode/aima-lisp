;;;; The N-Queens Puzzle as a Constraint Satisfaction Problem

(defstructure (nqueens-problem (:include CSP-problem)
			       (:constructor create-nqueens-problem))
  (n 8)
  (explicit? nil))

(defun make-nqueens-problem (&rest args &key (n 8) (explicit? nil))
  (apply #'create-nqueens-problem 
	 :initial-state (nqueens-initial-state n explicit?)
	 args))

(defun nqueens-initial-state (n &optional (explicit? nil) (complete? nil))
  (let ((s (make-CSP-state
	    :unassigned (mapcar #'(lambda (var)
				    (make-CSP-var :name var
						  :domain (iota n)))
				(iota n))
	    :assigned nil
	    :constraint-fn (if explicit?
			       (let ((constraints (nqueens-constraints n)))
				 #'(lambda (var1 val1 var2 val2)
				     (CSP-explicit-check
				      var1 val1 var2 val2 constraints)))
			     #'nqueens-constraint-fn))))
    (if complete? (CSP-random-completion s) s)))

(defun nqueens-constraints (n)
  (let ((constraints (make-array (list n n))))
    (dotimes (i n constraints)
      (dotimes (j n)
	(unless (= i j)
	  (dotimes (vi n)
	    (dotimes (vj n)
	      (unless (or (= vi vj)
			  (= (abs (- j i)) (abs (- vj vi))))
		(push (cons vi vj) 
		      (aref constraints i j))))))))))

(defun nqueens-constraint-fn (var1 val1 var2 val2)
  (not (or (= val1 val2)
	   (= (abs (- var1 var2)) (abs (- val1 val2))))))

		      