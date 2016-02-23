;;; Passive, stochastic 4x3 environment for chapter 20.
;;; Just one possible action (no-op), uniformly arrives at neighbour square.

(defvar *4x3-passive-M-data*)
(defvar *4x3-passive-R-data*)
(defvar *4x3-passive-mdp*)

(setq *4x3-passive-mdp*
      (make-mdp
       :model (make-hash-table :test #'equal)
       :rewards (make-hash-table :test #'equal)
       :initial-state '(1 1)
       :terminal-states '((4 2) (4 3))
       :name "4x3-passive-mdp"))

(setq *4x3-passive-M-data* '(
 ((1 1)   (no-op (((2 1) 0.5) ((1 2) 0.5))))
 ((1 2)   (no-op (((1 1) 0.5) ((1 3) 0.5))))
 ((1 3)   (no-op (((2 3) 0.5) ((1 2) 0.5))))
 ((2 1)   (no-op (((1 1) 0.5) ((3 1) 0.5))))
 ((2 3)   (no-op (((1 3) 0.5) ((3 3) 0.5))))
 ((3 1)   (no-op (((3 2) 0.333333) ((2 1) 0.333333) ((4 1) 0.333333))))
 ((3 2)   (no-op (((3 3) 0.333333) ((4 2) 0.333333) ((3 1) 0.333333))))
 ((3 3)   (no-op (((3 2) 0.333333) ((2 3) 0.333333) ((4 3) 0.333333))))
 ((4 1)   (no-op (((4 2) 0.5) ((3 1) 0.5))))
 ((4 2)   (no-op ()))
 ((4 3)   (no-op ()))
 ))

(setq *4x3-passive-R-data* '(((1 1) 0) ((1 2) 0) ((1 3) 0) 
			   ((2 1) 0) ((2 3) 0) 
			   ((3 1) 0) ((3 2) 0) ((3 3) 0)
			   ((4 1) 0) ((4 2) -1) ((4 3) 1)))


(dolist (sd *4x3-passive-M-data*) 
	(setf (gethash (car sd) (mdp-model *4x3-passive-mdp*)) (cdr sd)))
(dolist (sr *4x3-passive-R-data*) 
	(setf (gethash (car sr) (mdp-rewards *4x3-passive-mdp*)) (cadr sr)))
