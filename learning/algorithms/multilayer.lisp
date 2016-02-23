;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; back-propagation learning - multi-layer neural networks

;;; backprop-learning is the standard "induction algorithm"
;;; and interfaces to the learning-curve functions

(defun backprop-learning (problem 
			  &optional 
                           (hidden (length 
                                    (learning-problem-attributes problem))))
  (nn-learning problem
	       (make-connected-nn 
                (list (length (learning-problem-attributes problem))
                      hidden 
                      (length (learning-problem-goals problem))))
	       #'backprop-update))

;;; Backprop updating - Hertz, Krogh, and Palmer, p.117

(defun backprop-update (network actual-inputs predicted target
			  &optional (learning-rate 0.5)
			  &aux (all-inputs (cons -1 actual-inputs)))
  (backpropagate (reverse network) ;;; start at the output layer
		 all-inputs        ;;; include the bias input
		 (mapcar #'(lambda (iunit predicted-i target-i)
			     (* (unit-gradient iunit)
				(- target-i predicted-i)))
			 (car (last network)) predicted target)
		 learning-rate)
  network)

(defun backpropagate (rnetwork   ;;; network in reverse order
                      inputs     ;;; the inputs to the network
		      deltas     ;;; the "errors" for current layer
		      learning-rate)
  (cond ((null (cdr rnetwork))   ;;; have reached the earliest hidden layer
	 (backprop-update-layer
	  (car rnetwork) inputs deltas learning-rate))
	(t (backprop-update-layer
	    (car rnetwork) (cons -1 (mapcar #'unit-a (cadr rnetwork)))
	    deltas learning-rate)
	   (backpropagate 
	    (cdr rnetwork) 
	    inputs 
	    (compute-deltas (cadr rnetwork) (car rnetwork) deltas)
	    learning-rate))))


(defun backprop-update-layer (layer all-inputs deltas learning-rate)
  (mapc #'(lambda (unit delta)
	    (mapl #'(lambda (weights inputs)
		      (incf (car weights)
			    (* learning-rate
			       delta
			       (car inputs))))
		  (unit-weights unit) all-inputs))
	layer deltas))

;;; compute-deltas propagates the deltas back from layer i to layer j
;;; pretty ugly, partly because weights Wji are stored only at layer i

(defun compute-deltas (jlayer ilayer ideltas &aux (j 0))
  (mapcar #'(lambda (junit)
	      (incf j)
	      (* (unit-gradient junit)
		 (dot-product ideltas
			      (mapcar #'(lambda (iunit)
					  (nth j (unit-weights iunit)))
				      ilayer))))
	  jlayer))







