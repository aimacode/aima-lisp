;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Agents for the wumpus world

(defstructure (wumpus-agent (:include agent (body (make-wumpus-agent-body))))
  "The default wumpus agent gets an arrow.")

(defstructure (random-wumpus-agent
  (:include wumpus-agent
   (program
    #'(lambda (percept)
	(declare (ignore percept))
	(random-element '(forward (turn right) (turn left) shoot grab 
				  release climb)))))))

(defstruct (aimless-wumpus-agent
  (:include wumpus-agent
   (program
     (let ((plan nil)
	   (wumpus-alive? t))
     #'(lambda (percept)
	 (destructuring-bind (stench breeze glitter bump sound) percept
         (when sound
           (setf wumpus-alive? nil))
	 (cond (glitter 'grab)
	       (bump (setf plan '((turn right) (turn right) forward))
		     (pop plan))
	       (plan (pop plan))
               ((or breeze (and wumpus-alive? stench))
                (setf plan (list (random-element '((turn left) (turn right)))
                                 'forward))
                (pop plan))
	       (t (random-element '(forward forward (turn right)
					    (turn left)))))))))))
  "This agent does the obvious reactive things: grab when there's a glitter,
  and turn and move when there's a bump.  If the wumpus is alive and
  there's a stench, it turns and moves.  Otherwise it wanders aimlessly.")
