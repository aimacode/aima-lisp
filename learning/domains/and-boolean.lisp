;;; learning/domains/and-boolean.lisp
;;; Data for Boolean AND function


(defvar *and-boolean-problem*)

(setq *and-boolean-problem*
      (make-learning-problem
       :attributes '((a1 1 0)
                     (a2 1 0))
       :goals      '((g1 1 0))
       :examples   '(((G1 . 0) (A1 . 0) (A2 . 0))
		     ((G1 . 0) (A1 . 0) (A2 . 1))
		     ((G1 . 0) (A1 . 1) (A2 . 0))
		     ((G1 . 1) (A1 . 1) (A2 . 1))
		     )))

