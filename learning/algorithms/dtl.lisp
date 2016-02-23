;;; decision tree learning algorithm - the standard "induction algorithm"
;;; returns a tree in the format
;;;   (a1 (v11 . <tree>) (v12 . <tree>)), bottoming out with goal values.
;;; currently handles only a single goal attribute

(defun decision-tree-learning (problem)
  (dtl (learning-problem-examples problem)
       (learning-problem-attributes problem)
       (first (learning-problem-goals problem))))

(defun dtl (examples attributes goal &optional prior &aux (trees nil))
  (cond ((null examples) prior)
        ((null attributes) (majority examples goal))
        ((every #'(lambda (e) (eq (attribute-value goal e) 
				  (attribute-value goal (first examples))))
		(rest examples))
         (majority examples goal))
        (t (let ((best (select-attribute examples attributes goal)))
             (dolist (v (rest best) (cons best trees))
               (push (cons v (dtl (remove-if-not 
				    #'(lambda (e) (eq v (attribute-value best e)))
				    examples)
				  (remove best attributes)
				  goal
				  (majority examples goal)))
                     trees))))))

(defun distribution (examples goal
                     &aux (l (length (rest goal)))
		          (d (make-list l :initial-element 0)))
  (dolist (e examples) 
    (incf (nth (position (attribute-value goal e) (rest goal)) d)))
  (mapcar #'(lambda (n) (float (/ n (length examples)))) d))

(defun majority (examples goal)
  (the-biggest #'(lambda (v) 
		   (count v (mapcar #'(lambda (e) (attribute-value goal e))
				    examples))) 
	       (rest goal)))

(defun select-attribute (examples attributes goal)
  (the-biggest #'(lambda (a) (information-value a examples goal)) 
	       attributes))

(defun information-value 
  (a examples goal 
   &aux (i (bits-required (distribution examples goal))))
  (dolist (v (rest a) i)
    (let ((s (remove-if-not #'(lambda (e) (eq (attribute-value a e) v)) examples)))
      (when s (decf i (* (bits-required (distribution s goal))
                         (/ (length s) (length examples))))))))

(defun bits-required (d &aux (b 0))
  (dolist (p d (- b))
    (unless (= 0 p)
      (incf b (* p (log p 2))))))

;;; dtpredict is the standard "performance element" that 
;;; interfaces with the example-generation and learning-curve functions

(defun dtpredict (dt example)
  (if (atom dt) (list dt)
    (dtpredict (cdr (assoc (attribute-value (car dt) example) (cdr dt)))
	       example)))
