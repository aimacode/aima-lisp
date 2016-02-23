;;; learning:inductive-learning.lisp
;;; Definition for learning problems and
;;; utilities for generating and manipulating example data.
;;; Examples are list of (attribute . value) pairs.
;;; Some of these may be goal values -- no explicit separation in data


(defstruct (learning-problem (:print-function print-learning-problem))
  examples
  attributes
  goals)

(defun attribute-name (attribute) (first attribute))
(defun attribute-values (attribute) (rest attribute))

(defun attribute-value (attribute example)
  (cdr (assoc (attribute-name attribute) example :test #'eq)))

(defun random-examples (n attributes &aux (l nil))
  (dotimes (i n l)
    (push (mapcar #'(lambda (a)
		      (cons (attribute-name a)
			    (random-element (attribute-values a))))
		  attributes)
	  l)))

(defun classify (unclassified-examples goals h performance-element)
  (mapcar #'(lambda (e)
	      (append (mapcar #'(lambda (goal value) 
				(cons (attribute-name goal)
				      value))
			    goals
			    (funcall performance-element h e))
		    e))
	  unclassified-examples))

(defun consistent (examples goals h performance-element)
  (every #'(lambda (e)
	      (every #'(lambda (goal value) 
				(eq (attribute-value goal e)
				    value))
		     goals
		     (funcall performance-element h e)))
	  examples))

;;; Coded examples have goal values (in a single list)
;;; followed by attribute values, both in fixed order

(defun code-examples (examples attributes goals)
  (mapcar #'(lambda (e) (code-example e attributes goals)) examples))

(defun code-example (example attributes goals)
  (cons (mapcar #'(lambda (g) (attribute-value g example)) goals)
	(mapcar #'(lambda (a) (attribute-value a example)) attributes)))

(defun code-unclassified-example (example attributes goals)
  (declare (ignore goals))
  (mapcar #'(lambda (a) (attribute-value a example)) attributes))

(defun print-learning-problem (problem &optional stream depth)
  (declare (ignore depth))
  (format stream "#<~A with ~D examples and ~D attributes>"
	  (type-of problem)
	  (length (learning-problem-examples problem))
	  (length (learning-problem-attributes problem))))
