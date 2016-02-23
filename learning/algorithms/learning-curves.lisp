;;; Functions for testing induction algorithms
;;; Tries to be as generic as possible
;;; Mainly for NN purposes, allows multiple goal attributes
;;; A prediction is correct if it agrees on ALL goal attributes

(defun learning-curve 
  (induction-algorithm ;;; examples -> hypothesis
   performance-element ;;; hypothesis + example -> prediction
   examples attributes goals trials training-size-increment 
   &optional (error-fn #'boolean-error)
   &aux training-set test-set (training-set-size 0)
        (points (- (floor (length examples) training-size-increment) 1))
        (results nil))
  (dotimes (i points (reverse results))
    (incf training-set-size training-size-increment)
    (push (cons training-set-size 0) results)
    (dotimes (trial trials)
      (setq training-set 
	    (sample-without-replacement training-set-size examples))
      (setq test-set (remove-if 
		      #'(lambda (e) (member e training-set :test #'eq))
		      examples))
      (incf (cdar results) 
	    (accuracy 
	     (funcall induction-algorithm training-set attributes goals)
	     performance-element test-set goals error-fn)))
    (setf (cdar results) (/ (cdar results) trials))))


;;; this version uses incremental data sets rather than a new batch each time
(defun incremental-learning-curve 
  (induction-algorithm ;;; examples -> hypothesis
   performance-element ;;; hypothesis + example -> prediction
   examples attributes goals trials training-size-increment 
   &optional (error-fn #'boolean-error)
   &aux training-set test-set (training-set-size 0)
        (points (- (floor (length examples) training-size-increment) 1))
        (results nil))
  (dotimes (i points)
    (incf training-set-size training-size-increment)
    (push (cons training-set-size 0) results))
  (dotimes (trial trials)
    (setf training-set-size 0)
    (setq test-set examples)
    (setq training-set nil)
    (dotimes (i points)
      (incf training-set-size training-size-increment)
      (setq training-set 
	    (append (sample-without-replacement 
		     training-size-increment test-set)
		    training-set))
      (setq test-set (remove-if 
		      #'(lambda (e) (member e training-set :test #'eq))
		      test-set))
      (incf (cdr (assoc training-set-size results))
	    (accuracy 
	     (funcall induction-algorithm training-set attributes goals)
		      performance-element test-set goals error-fn))))
  (dolist (xy results)
    (setf (cdr xy) (/ (cdr xy) trials)))
  (reverse results))


(defun accuracy (h performance-element test-set goals 
		  &optional (error-fn #'boolean-error))
  (float (/ (sum test-set
	         #'(lambda (e)
		     (- 1 (funcall error-fn
				   (funcall performance-element h e)
				   (mapcar #'(lambda (g) 
					       (attribute-value g e))
					   goals)))))
	    (length test-set))))

