;;; File: shopping.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Shopping World: 

;;; Warning!  This code has not yet been tested or debugged!

(defparameter *page250-supermarket*
    '((at edge wall)   
      (at (1 1) (sign :words (exit)))
      (at (and (2 2) (6 2)) shopper)
      (at (and (3 2) (7 2)) cashier-stand)
      (at (and (4 2) (8 2) (4 7)) cashier)

      (at (2 4) (sign :words (Aisle 1 Vegetables)))
      (at (2 5) (-15 tomato) (sign :words (Tomatoes $ .79 lb)))
      (at (2 6) (-6 lettuce) (sign :words (Lettuce $ .89)))
      (at (2 7) (-8 onion)   (sign :words (Onion $ .49 lb)))

      (at (3 4) (sign :words (Aisle 2 Fruit)))
      (at (3 5) (-12 apple)  (sign :words (Apples $ .69 lb)))

      (at (3 6) (-9 orange)  (sign :words (Oranges $ .75 lb)))
      (at (3 7) (-3 grapefruit :size 0.06  :color yellow)
       (-3 grapefruit :size 0.07 :color pink)
       (sign :words (Grapefruit $ .49 each)))

      ;; The rest of the store is temporarily out of stock ...
      (at (5 4) (sign :words (Aisle 3 Soup Sauces)))
      (at (6 4) (sign :words (Aisle 4 Meat)))
      (at (8 4) (sign :words (Aisle 5 Sundries)))
      ))

(defstructure (shopping-world (:include grid-environment
    (aspec '(shopping-agent))
    (bspec *page250-supermarket*))))

;;;; New Structures

(defstructure (credit-card (:include object (name "$"))))
(defstructure (food (:include object (shape :round) (size .1) (name 'f))))
(defstructure (tomato (:include food (color 'red) (size .08) (name 't))))
(defstructure (lettuce (:include food (color 'green) (size .09) (name 'l))))
(defstructure (onion (:include food (color 'yellow) (size .07) (name 'o))))
(defstructure (orange (:include food (color 'orange) (size .07) (name 'o))))
(defstructure (apple (:include food (color 'red) (size .07) (name 'a))))
(defstructure (grapefruit (:include food (color 'yellow) (size .1) (name 'g))))
(defstructure (sign (:include object (name 'S) (size .09)
                  (color '(white (with black)))))
  (words '()))
(defstructure (cashier-stand (:include object (color '(black (with chrome))) 
		           (shape 'flat) (size .9) (name 'C))))
(defstructure (cashier (:include agent-body (name "c"))))
(defstructure (seeing-agent-body (:include agent-body (name ":")))
  (zoomed-at nil)    ; Some have a camera to zoom in and out at a location
  (can-zoom-at '((0 0) (0 +1) (+1 +1) (-1 +1)))
  (visible-offsets '((0 +1) (+1 +1) (-1 +1))))
(defstructure (shopper (:include seeing-agent-body (name "@")
                     (contents (list (make-credit-card))))))

;;;; Percepts

(defmethod get-percept ((env shopping-world) agent)
  "The percept is a sequence of sights, touch (i.e. bump), and sounds."
  (list (see agent env)	(feel agent env) (hear agent env)))

(defun see (agent env)
  "Return a list of visual percepts for an agent.  Note the agent's camera may
  either be zoomed out, so that it sees several squares, or zoomed in on one."
  (let* ((body (agent-body agent))
         (zoomed-at (seeing-agent-body-zoomed-at body)))
    (mappend #'(lambda (offset)
		 (see-loc (absolute-loc body offset) env zoomed-at))
	     (seeing-agent-body-visible-offsets body))))

(defun feel (agent env)
  (declare (ignore env))
  (if (object-bump (agent-body agent)) 'bump))

(defun hear (agent env)
  ;; We can hear anything within 2 squares
  (let* ((body (agent-body agent))
         (loc (object-loc body))
	 (objects nil))
     (for each obj in (grid-environment-objects env) do
          (when (and (object-sound obj) (near? (object-loc obj) loc 2))
	    (push (object-sound obj) objects)))
     objects))

(defun see-loc (loc env zoomed-at)
  (let ((objects (grid-contents env loc)))
    (if zoomed-at
        (mappend #'appearance objects)
      (appearance objects))))

(defun appearance (object)
  "Return a list of visual attributes: (loc size color shape words)"
  (list (object-loc object) (fuzz (object-size object)) (object-color object)
	(object-shape object) (object-words object)))

(defun object-words (object)
  (if (sign-p object)
      (sign-words object)
    nil))

(defun zoom (agent-body env offset)
  "Zoom the camera at an offset if it is feasible; otherwise zoom out."
  (declare (ignore env))
  (cond ((member offset (seeing-agent-body-can-zoom-at agent-body))
	 (setf (seeing-agent-body-zoomed-at agent-body) offset)
	 (setf (seeing-agent-body-visible-offsets agent-body) (list offset)))
	(t ;; Zoom out
	   (setf (seeing-agent-body-zoomed-at agent-body) nil)
	   (setf (seeing-agent-body-visible-offsets agent-body)
                 (remove '(0 0) (seeing-agent-body-can-zoom-at agent-body)
                         :test #'equal)))))





