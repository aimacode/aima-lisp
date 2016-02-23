;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; perceptron learning - single-layer neural networks

;;; make-perceptron returns a one-layer network with m units, n inputs each

(defun make-perceptron (n m &optional (g  #'(lambda (i) (step-function 0 i)))
			    &aux (l nil))
  (dotimes (i m (list l))
    (push (make-unit :parents (iota (1+ n))
		     :children nil
		     :weights (random-weights (1+ n) -0.5 +0.5)
		     :g g)
	  l)))

(defun majority-perceptron (n &optional (g  #'(lambda (i) (step-function 0 i))))
  (list (list (make-unit :parents (iota (1+ n))
			 :children nil
			 :weights (cons (/ n 4) 
					(make-list n :initial-element 0.5))
			 :g g))))


;;; perceptron-learning is the standard "induction algorithm"
;;; and interfaces to the learning-curve functions

(defun perceptron-learning (problem)
  (nn-learning problem
	       (make-perceptron 
                (length (learning-problem-attributes problem))
                (length (learning-problem-goals problem)))
	       #'perceptron-update))

;;; Perceptron updating - simple version without lower bound on delta
;;; Hertz, Krogh, and Palmer, eq. 5.19 (p.97)

(defun perceptron-update (perceptron actual-inputs predicted target
			  &optional (learning-rate 0.1)
			  &aux (all-inputs (cons -1 actual-inputs)))
  (mapc #'(lambda (unit predicted-i target-i)
	    (mapl #'(lambda (weights inputs)
		      (incf (car weights)
			    (* learning-rate
			       (- target-i predicted-i)
			       (car inputs))))
		  (unit-weights unit) all-inputs))
	(car perceptron) predicted target)
  perceptron)



