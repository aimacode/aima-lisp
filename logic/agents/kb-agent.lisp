;;; -*- Mode: Lisp; -*- Author: Peter Norvig

;;;; Knowledge-Based Agent

(defstructure (action-value-agent (:include agent)
				  (program (make-action-value-agent-program))))

(defun make-action-value-agent-program (&key (kb (make-fol-kb)))
  "Define an action-value knowledge-based agent. [p 210]"
  (let ((t 0))
    #'(lambda (percept)
	(tell kb `(Percept ,percept ,t))
	(let ((action (or (ask-pattern kb `(Great $a t) '$a)
			  (ask-pattern kb `(Good $a t) '$a)
			  (ask-pattern kb `(Medium $a t) '$a)
			  (ask-pattern kb `(Risky $a t) '$a))))
	  (tell kb `(Did Self ,action ,t))
	  (incf t)
	  action))))

; p 177