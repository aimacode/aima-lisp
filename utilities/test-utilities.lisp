;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/test.lisp

;;;; Test cases for the basic utilities

(deftest utilities
  "Test all the utility functions."

  "Test the CLOS implementation"
  ((defstructure shape))
  ((defstructure (triangle (:include shape))
     base height))
  ((defstructure (rectangle (:include shape))
     height width))
  ((defmethod area ((x triangle))
     (* 1/2 (triangle-base x) (triangle-height x))))
  ((defmethod area ((x rectangle))
     (* (rectangle-height x) (rectangle-width x))))
  ((area (make-triangle :base 10 :height 10)) (equal * 50))
  ((area (make-rectangle :width 10 :height 10)) (equal * 100))
  ((defmethod features ((x shape)) 
     '(shapely)))
  ((defmethod features ((x triangle))
     (cons (if (eql 0 (triangle-base x)) 'line 'triangle)
	   (call-next-method))))
  ((features (make-triangle :base 0 :height 10)) (equal * '(line shapely)))
  ((features (make-triangle :base 1 :height 10)) (equal * '(triangle shapely)))

  "Now, some operations on lists."
  ((length>1 '(a b c)) *)
  ((random-element '(a b c)) (member * '(a b c)))
  ((mappend #'reverse '((a b c) (1 2 3))) (equal * '(c b a 3 2 1)))
  ((starts-with '(hi there) 'hi) *)
  ((last1 '(a b c)) (eq * 'c))
  ((transpose '((a b c) (d e f))) (equal * '((a d) (b e) (c f))))
  ((setq l '(a b c)))
  ((deletef 'a l) (equal l '(b c)))

  "Now for 2-dimensional points."
  ((xy-add (@ 1 2) (@ 10 20)) (equal * (@ 11 22)))
  ((xy-distance (@ 0 0) (@ 3 4)) (= * 5))

  "Numeric utilities"
  ((average '(10 20 30)) (= * 20))
  ((sum '(10 20 30)) (= * 60))
  ((sum '(1 2 3) #'square) (= * 14))
  ((random-integer 8 10) (member * '(8 9 10)))
  ((fuzz 10) (<= 9 * 11))
  ((round-off 3.14159 .01) (< 3.139 * 3.141))

  "Other"
  ((stringify '(a b c)) (equalp * "(A B C)"))
  ((concat-symbol 'a 1) (eq * 'a1))
  ((funcall (compose #'- #'sqrt) 16) (= * -4))
  ((setq nums '(1 2 3 4 -5 -2 -1)))
  ((the-biggest #'identity nums) (eql * 4))
  ((the-biggest #'abs nums) (eql * -5))
  ((the-biggest-that #'identity #'oddp nums) (eql * 3))
  ((the-smallest-random-tie #'abs nums) (member * '(1 -1)))

  "Now test the priority queue code."
  ((heap-sort '(1 4 3 5 2 0)) (equal * '(0 1 2 3 4 5)))
  ((heap-sort '(1 4 3 5 2 6) :key #'-) (equal * '(6 5 4 3 2 1)))

  "Now destructuring-bind"
  ((destructuring-bind ((a . b) c &rest d &key e (f 5)) '((1 . 2) 3 :e 4)
     (list a b c d e f)) (equal * '(1 2 3 (:e 4) 4 5)))
  )
