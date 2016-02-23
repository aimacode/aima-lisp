;;;; Stochastic active 4x3 world for chapters 17, 20.

;;; Each action achieves the intended effect with probability 0.8, but the
;;; rest of the time, the action moves the agent at right angles to the
;;; intended direction.  For example, from the start square (1,1), the
;;; action North moves the agent to (1,2) with probability 0.8, but with
;;; probability 0.1, it moves East to (2,1), and with probability 0.1,
;;; it moves West, bumps into the wall, and stays in (1,1).

(defparameter *4x3-mdp*
  (make-mdp
   :initial-state '(1 1)
   :terminal-states '((4 2) (4 3))
   :name "4x3-mdp"))

(defparameter *4x3-M-data* '(
 ((1 1)   (left (((1 1) 0.9) ((1 2) 0.1)))
          (right (((2 1) 0.8) ((1 2) 0.1) ((1 1) 0.1)))
	  (up (((1 2) 0.8) ((2 1) 0.1) ((1 1) 0.1)))
	  (down (((1 1) 0.9)  ((2 1) 0.1))))
 ((1 2)   (left (((1 2) 0.8) ((1 1) 0.1) ((1 3) 0.1)))
          (right (((1 2) 0.8) ((1 1) 0.1) ((1 3) 0.1)))
	  (up (((1 2) 0.2)  ((1 3) 0.8)))
	  (down (((1 2) 0.2) ((1 1) 0.8))))
 ((1 3)   (left (((1 3) 0.9)  ((1 2) 0.1)))
          (right (((1 3) 0.1) ((2 3) 0.8) ((1 2) 0.1))) 
	  (up (((1 3) 0.9) ((2 3) 0.1))) 
	  (down (((1 3) 0.1) ((2 3) 0.1) ((1 2) 0.8))))
 ((2 1)   (left (((2 1) 0.2) ((1 1) 0.8)))
          (right (((2 1) 0.2)  ((3 1) 0.8)))
	  (up (((2 1) 0.8) ((1 1) 0.1) ((3 1) 0.1)))
	  (down (((2 1) 0.8) ((1 1) 0.1) ((3 1) 0.1))))
 ((2 3)   (left (((2 3) 0.2) ((1 3) 0.8))) 
          (right (((2 3) 0.2)   ((3 3)  0.8))) 
	  (up (((2 3) 0.8) ((1 3) 0.1)  ((3 3)  0.1))) 
	  (down (((2 3) 0.8) ((1 3) 0.1)  ((3 3)  0.1))))
 ((3 1)   (left (((3 1) 0.1) ((3 2) 0.1) ((2 1) 0.8))) 
          (right (((3 1) 0.1) ((3 2) 0.1) ((4 1) 0.8))) 
	  (up ( ((3 2) 0.8) ((2 1) 0.1) ((4 1) 0.1))) 
	  (down (((3 1) 0.8)  ((2 1) 0.1) ((4 1) 0.1))))
 ((3 2)   (left (((3 2) 0.8) ((3 1) 0.1)  ((3 3)  0.1))) 
          (right (((4 2) 0.8) ((3 1) 0.1)  ((3 3)  0.1))) 
	  (up (((3 2) 0.1) ((4 2) 0.1)  ((3 3)  0.8))) 
	  (down (((3 2) 0.1) ((4 2) 0.1)  ((3 1)  0.8))))
 ((3 3)   (left (((2 3) 0.8) ((3 3) 0.1)  ((3 2)  0.1))) 
          (right (((3 2) 0.1) ((4 3) 0.8)  ((3 3)  0.1))) 
	  (up (((2 3) 0.1) ((4 3) 0.1)  ((3 3)  0.8))) 
	  (down (((3 2) 0.8) ((2 3) 0.1)  ((4 3)  0.1))))
 ((4 1)   (left (((4 1) 0.1) ((3 1) 0.8)  ((4 2)  0.1))) 
          (right (((4 1) 0.9) ((4 2) 0.1))) 
	  (up (((4 2) 0.8) ((4 1) 0.1)  ((3 2)  0.1))) 
	  (down (((4 1) 0.9) ((3 1) 0.1))))
 ((4 2)   (left ())
          (right ())
	  (up ())
	  (down ()))
 ((4 3)   (left ())
          (right ())
	  (up ())
	  (down ()))
 ))

(defparameter *4x3-R-data* 
  '(((1 1) -0.04) ((1 2) -0.04) ((1 3) -0.04) 
    ((2 1) -0.04)               ((2 3) -0.04) 
    ((3 1) -0.04) ((3 2) -0.04) ((3 3) -0.04)
    ((4 1) -0.04) ((4 2) -1)    ((4 3) 1)))


(dolist (sd *4x3-M-data*) 
  (setf (gethash (car sd) (mdp-model *4x3-mdp*)) (cdr sd)))
(dolist (sr *4x3-R-data*) 
  (setf (gethash (car sr) (mdp-rewards *4x3-mdp*)) (second sr)))

