;;; decision list learning algorithm (Rivest)
;;; returns a decision list, each element of which is 
;;; a test of the form (x .term), where each term is
;;; of the form ((a1 . v1) (a2 . v2) ... (an . vn)).
;;; The last element is the test (0).
;;; only works for purely boolean attributes.

(defun decision-list-learning (k problem)
 (dll k (learning-problem-examples problem)
        (learning-problem-attributes problem)
        (first (learning-problem-goals problem))))

(defun dll (k examples attributes goal)
  (if (null examples) 
      (list (list 0))
    (multiple-value-bind (test subset)
        (select-test k examples attributes goal)
      (if test
	  (cons test
		(dll k (set-difference examples subset :test #'eq) attributes goal))
	(error "Cannot find a consistent decision list")))))

;;; select-test finds a test of size at most k that picks out a set of
;;; examples with uniform classification. Returns test and subset.

(defun select-test (k examples attributes goal)
  (dotimes (i (1+ k) (values nil nil))
    (let ((test (select-k-test i examples attributes goal nil)))
      (when test 
	    (return (values test 
			    (remove-if-not #'(lambda (e) (passes e test)) 
					   examples)))))))

(defun select-k-test (k examples attributes goal test-attributes)
  (cond ((= 0 k)
	 (dolist (term (generate-terms test-attributes) nil)
	   (let ((subset (remove-if-not 
			  #'(lambda (e) (passes e (cons 0 term)))
			  examples)))
	     (when (and subset (uniform-classification subset goal))
	       (return (cons (attribute-value goal (first subset)) term))))))
	(t 
	 (dolist (f attributes nil)
	   (let ((test (select-k-test (- k 1) 
				      examples 
				      (remove f attributes :test #'eq) 
				      goal
				      (cons f test-attributes))))
	     (when test (return test)))))))

(defun generate-terms (attributes) ;;; generate all labellings
  (if (null attributes)
      (list nil)
    (let ((rest (generate-terms (cdr attributes))))
      (nconc (mapcar #'(lambda (test) 
			 (cons (cons (car attributes) 0) test))
		     rest)
	     (mapcar #'(lambda (test) 
			 (cons (cons (car attributes) 1) test))
		     rest)))))

(defun uniform-classification (examples goal)
  (every #'(lambda (e) (eq (attribute-value goal e) 
			   (attribute-value goal (first examples))))
	 (rest examples)))

(defun passes (example test)
  (every #'(lambda (av) 
	     (eq (attribute-value (car av) example) (cdr av)))
	 (cdr test)))


;;; dlpredict is the standard "performance element" that 
;;; interfaces with the example-generation and learning-curve functions

(defun dlpredict (dl example)
  (if (every #'(lambda (av) (eq (attribute-value (car av) example) (cdr av)))
	     (cdar dl))
      (list (caar dl))
    (dlpredict (cdr dl) example)))
