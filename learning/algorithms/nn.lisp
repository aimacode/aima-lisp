;;; Code for layered feed-forward networks
;;; Network is represented as a list of lists of units.
;;; Inputs assumed to be the ordered attribute values in examples
;;; Every unit gets input 0 set to -1


(defstruct unit parents   ;;; sequence of indices of units in previous layer
                children  ;;; sequence of indices of units in subsequent layer
		weights   ;;; weights on links from parents
		g         ;;; activation function
		(dg nil)  ;;; activation gradient function g' (if it exists)
		a         ;;; activation level
                in        ;;; total weighted input
		gradient  ;;; g'(in_i)
		)

;;; make-connected-nn returns a multi-layer network with layers given by sizes

(defun make-connected-nn (sizes &optional (previous nil)
                    			  (g  #'sigmoid) 
				          (dg #'(lambda (x) 
						  (let ((gx (funcall g x)))
						    (* gx (- 1 gx)))))
				&aux (l nil))
  (cond ((null (cdr sizes)) nil)
	(t (when previous
	     (dolist (unit previous)
               (setf (unit-children unit) (iota (cadr sizes) 1))))
	   (dotimes (i (cadr sizes))
             (push (make-unit :parents (iota (1+ (car sizes)))
			      :children nil
			      :weights (random-weights (1+ (car sizes)) -0.5 +0.5)
			      :g g :dg dg)
		   l))
	   (cons l (make-connected-nn (cdr sizes) l)))))


(defun step-function (threshold x)
  (if (> x threshold) 1 0))

(defun sign-function (threshold x)
  (if (> x threshold) 1 -1))

(defun sigmoid (x)
  (/ 1 (1+ (exp (- x)))))

;;; nn-learning establishes the basic epoch struture for updating,
;;; Calls the desired updating mechanism to improve network until 
;;; either all correct or runs out of epochs
                                                                            
(defun nn-learning (problem 
		    network learning-method
		    &key 
                     (tolerance (* 0.01 
                                   (length (learning-problem-examples problem))))
		    (limit 1000)
                    &aux all-correct error
                         (examples (learning-problem-examples problem))
                         (attributes (learning-problem-attributes problem))
                         (goals (learning-problem-goals problem))
		         (coded-examples 
                          (code-examples examples attributes goals)))
  (dotimes (epoch limit network)
    (setq all-correct t)
    (setq error (nn-error coded-examples network))
    (dprint (list 'epoch epoch 'error error))
    (when (< error tolerance) (return network))
    (dolist (e coded-examples)
      (let ((target (car e)) 
	    (predicted (network-output (cdr e) network)))
	(setq all-correct (and all-correct (equal target predicted)))
	(setq network (funcall learning-method network (cdr e) 
			       predicted target))))
    (when all-correct (return network))))

(defun nn-error (examples network &aux (sum 0))
    (dolist (e examples (* 0.5 sum))
      (let ((target (car e)) 
	    (predicted (network-output (cdr e) network)))
	(mapc #'(lambda (x y) (incf sum (square (- x y))))
	      predicted target))))
  

(defun network-output (inputs network)
  (dolist (layer network inputs)
    (setq inputs 
     (mapcar #'(lambda (unit) 
		 (unit-output (get-unit-inputs inputs (unit-parents unit))
			      unit)) 
	     layer))))

;;; nn-output is the standard "performance element" for neural networks
;;; and interfaces to example-generating and learning-curve functions.
;;; Since performance elements are required to take only two arguments
;;; (hypothesis and example), nn-output is used in an appropriate
;;; lambda-expression

(defun nn-output (network unclassified-example attributes goals)
  (network-output (code-unclassified-example unclassified-example
					     attributes goals) 
		  network))

		  
  
;;; unit-output computes the output of a unit given a set of inputs  
;;; it always adds a bias input of -1 as the zeroth input

(defun unit-output (inputs unit)
  (setf (unit-a unit)
	(funcall (unit-g unit)
		 (setf (unit-in unit)
		       (dot-product (unit-weights unit) (cons -1 inputs)))))
;  (when (unit-dg unit) ;;; this is the general way to do it
;    (setf (unit-gradient unit)
;	  (funcall dg (unit-in unit))))
  ;;; the following is specific to sigmoids
  (setf (unit-gradient unit) (* (unit-a unit) (- 1 (unit-a unit))))
  (unit-a unit))

(defun get-unit-inputs (inputs parents)
  (mapcar #'(lambda (parent) (nth parent inputs)) parents))

(defun random-weights (n low high &aux (l nil))
  (dotimes (i n l) 
    (push (+ low (random (- high low))) l)))

;;; print-nn prints out the network relatively prettily

(defun print-nn (network &aux i)
  (print (cons 'inputs (iota (length (unit-weights (caar network))))))
  (dolist (layer network)
    (print 'layer)
    (setq i 0)
    (dolist (unit layer)
      (incf i)
      (terpri) (princ "   ") (princ (list 'unit i 'weights))
      (dolist (w (unit-weights unit)) (format t "~7,3F" w)))))

