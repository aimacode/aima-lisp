;;; File: search/domains/vacuum.lisp

;;;; Definitions for Searching in the Vacuum-World Domain

(defstruct (vacuum-state (:type list))
  orientation                  ; an xy unit vector
  dirt                         ; list of dirt locations
  m n                          ; world is m by n squares
  on                           ; true if agent is switched on
  location                     ; xy agent location 
  )

(defvar *vacuum-home* (@ 0 0))

;;;;  Vacuum problem generator, and vacuum domain functions

(defun vacuum-problem (m n &optional (dirt nil) (dirt-probability 0.2))
  "Create a Vacuum World problem."
  (make-problem
   :initial-state      (vacuum-initial-state m n dirt dirt-probability)
   :successor-fn       #'vacuum-successors
   :goal-test          #'vacuum-goalp
   :h-cost-fn          #'(lambda (state)
			   (* 2 (length (vacuum-state-dirt state))))
   :domain             "vacuum world"
   ))

(defun vacuum-initial-state (m n dirt dirt-probability)
  (make-vacuum-state :orientation (@ 1 0)
		     :m m :n n :on t :location *vacuum-home*
		     :dirt (or dirt
			       (dotimes (x n dirt) 
				 (dotimes (y m)
				   (when (< (random 1.0) dirt-probability)
				     (push (@ x y) dirt)))))))

(defun vacuum-goalp  (state)
  "Is this a goal state?"
  (and (null (vacuum-state-dirt state))
       (xy-equal (vacuum-state-location state) *vacuum-home*)
       (not (vacuum-state-on state))))

(defun vacuum-successors (state)
  "Return a list of (action . state) pairs."
  (destructuring-bind (o d m n on l) state
    (if on
	(list
	 (cons 'forward 
	       (if (inside (xy-add l o) n m)
		   (make-vacuum-state :orientation o
				      :dirt d
				      :m m :n n :on on
				      :location (xy-add l o))
		 state))
	 (cons '(turn left)
	       (make-vacuum-state :orientation (rotate o 0 -1 1 0)
				  :dirt d
				  :m m :n n :on on
				  :location l))
	 (cons '(turn right)
	       (make-vacuum-state :orientation (rotate o 0 1 -1 0)
				  :dirt d
				  :m m :n n :on on
				  :location l))
	 (cons 'suck
	       (if (member l d :test #'xy-equal)
		   (make-vacuum-state :orientation o
				      :dirt (remove l d :test #'xy-equal)
				      :m m :n n :on on
				      :location l)
		 state))
	 (cons 'shut-off
	       (make-vacuum-state :orientation o
				  :dirt d
				  :m m :n n :on nil
				  :location l)))
      nil)))
