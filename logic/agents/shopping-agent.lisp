;;; -*- Mode: Lisp; -*- Author: Peter Norvig

;;;; Agents for the Shopping World

(defstructure (shopping-agent 
	       (:include agent
			 (program 'ask-human-shopping-program))))

(defun ask-human-shopping-program (percept)
  (format t "~&Agent ~%~@[Feels: ~A~%~]~{~^Hears: ~A~%~}~{~^Sees: ~A~%~}"
	  (second percept) (third percept) (first percept))
  (format t "ACTION: ")
  (read))
