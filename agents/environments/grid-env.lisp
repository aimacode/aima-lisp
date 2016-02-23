;;; File: grid-env.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Environments with a two-dimensional grid layout occupied by objects

;;; This file defines a GRID-ENVIRONMENT, a kind of environment where there is
;;; a rectangular grid of spaces, each potentially containing objects.
;;; (Notice that the ENVIRONMENT makes no mention of space or objects.)  You
;;; won't be creating instances of grid-environment directly, but it is the
;;; key structure that is inherited by the Vacuum, Wumpus, Shopping and
;;; Elevator worlds.

(defstructure (grid-environment (:include environment))
  (size (@ 10 10))          ; Size of the 2-D array
  (grid nil)		    ; Will be a 2-D array of squares
  (objects '())             ; List of objects currently in this env.
  (start (@ 1 1))	    ; Where agents begin
  (aspec '(ask-user-agent)) ; Specify default list of Agents
  (bspec '((at edge wall))) ; Specify Basic objects, common to all envs.
  (cspec '())               ; Specify objects that Change for each env.
  )

(defstructure object
  "An object is anything that occupies space.  Some objects are 'alive'."
  (name "?")			; Used to print the object on the map
  (alive? nil)                  ; Is the object alive?
  (loc (@ 1 1))			; The square that the object is in
  (bump nil)			; Has the object bumped into something?
  (size 0.5)			; Size of object as proportion of loc
  (color 'black)		; Some objects have a color
  (shape 'rectangle)		; Some objects have a shape
  (sound nil)			; Some objects create a sound
  (contents '())		; Some objects contain others
  (max-contents 0.4)            ; How much (total size) can fit inside?
  (container nil)		; Some objects are contained by another
  (heading (@ 1 0))		; Direction object is facing as unit vector
  )

(defstruct (obstacle (:include object (name "#"))))

(defstruct (wall (:include obstacle)))

(defstruct (agent-body (:include object (alive? t) (name nil)))
  "An agent body is an object; some bodies have a hand that can hold 1 thing."
  (holding nil))

;;;; Generic Functions

(defmethod update-fn ((env grid-environment))
  "Execute the actions and do bookkeeping on the bump sensor."
  (for each agent in (environment-agents env) do
       (setf (object-bump (agent-body agent)) nil)) ; dissipate bumps
  (execute-agent-actions env))

(defmethod legal-actions ((env grid-environment))
  '(turn forward grab release))

(defmethod initialize ((env grid-environment))
  "Build a new environment with all the agents and objects in place.
  This gets passed an environment which may need to have the objects placed.
  See PARSE-SPECS below in this file for more on initialization."
  (unless (environment-initialized env)
    ;; Build the grid and place objects where they belong
    (setf (grid-environment-grid env) 
	  (make-array (grid-environment-size env) :initial-element '()))
    (parse-specs env (grid-environment-aspec env))
    (parse-specs env (grid-environment-bspec env))
    (parse-specs env (grid-environment-cspec env))
    (call-next-method)))

(defmethod termination? ((env grid-environment)) 
  "By default, we stop when there are no live agents."
  (every #'(lambda (agent) (not (object-alive? (agent-body agent))))
	 (environment-agents env)))

(defmethod display-environment-snapshot ((env grid-environment))
  "Show what is in each location in the environment."
  (print-grid (grid-environment-grid env) :width 4
	      :stream (environment-stream env)
	      :key #'(lambda (objects)
		       (format nil "~{~A~}" objects))))

(defmethod print-structure ((object object) stream)
  "Show an object's name, and if it is alive, the direction it faces."
  (let ((name (or (object-name object) (type-of object))))
    (if (object-alive? object)
	(format stream "~A~A" name (heading->string (object-heading object)))
      (format stream "~A" name))))

;;;; Actions 

(defmethod speak ((env grid-environment) agent-body sound)
  "The agent emits a sound."
  (declare-ignore env)
  (setf (object-sound agent-body) sound))

(defmethod turn ((env grid-environment) agent-body direction)
  "The agent changes its heading by turning right or left."
  (declare-ignore env)
  (let* ((headings '#((1 0) (0 1) (-1 0) (0 -1)))
	 (now (position (agent-body-heading agent-body) headings
			:test #'equal))
	 (delta (case direction (right -1) (left +1) (t 0))))
    (setf (object-heading agent-body)
	  (elt headings (mod (+ now delta) 4)))))

(defmethod forward ((env grid-environment) agent-body)
  "Move the object to the location that is one step directly ahead of it."
  (move-object-to 
   agent-body 
   (add-locs (object-loc agent-body) (object-heading agent-body))
   env))

(defmethod grab ((env grid-environment) agent-body &optional args)
  "Grab an object at the specified location.  Assumes a one-handed agent."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (find-object-if #'grabable? (object-loc agent-body) env)))
    (when (and object 
	       (not (agent-body-holding agent-body))
	       (place-in-container object agent-body env))
      (setf (agent-body-holding agent-body) object))))

(defun grabable? (object)
  (and (not (obstacle-p object)) (not (agent-body-p object))))

(defmethod release ((env grid-environment) agent-body &optional args)
  "Release an object that is in the hand, putting it at the specified loc."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (agent-body-holding agent-body)))
    (when object
      (place-object object (object-loc agent-body) env)
      (setf (agent-body-holding agent-body) nil))))

;;;; Initializing Environments

;;; The grammar for the object-specs language is as follows:             
;;;<PRE>
;;;   specs  =>  (spec...)
;;;   spec   =>  (AT where what...) | (* n spec...) | what
;;;   where  =>  EDGE | ALL | FREE? | START | (x y) | (AND where...)
;;;   what   =>  object | type | (type arg...) | (* n what...)  | (P x what...)
;;;   n      =>  integer | (+- integer integer)
;;; 
;;; The location FREE? means a randomly chosen free loc, ALL means every loc.
;;; If no location is specified, the default is START for agents, FREE?
;;; otherwise.  
;;; 
;;; Examples of spec:
;;; 
;;;  (at edge wall)                  1 wall in every perimeter location
;;;  (at free? wumpus)               1 wumpus in some random free location
;;;  wumpus                          Same as above 
;;;  ask-user-agent                  1 ask-user-agent in the start cell
;;;  (* 2 apple)                     An apple in each of 2 random locations
;;;  (* 2 (apple :color green))      A green apple in each of 2 random locs
;;;  (at all (p 0.25 dirt))          All free locations have 1/4 chance of dirt
;;;  (at (2 3) (* 8 apple) sign)     Location (2 3) has 8 apples and a sign
;;;  (* (+- 10 4) apple)             10 plus or minus 4 (at random) apples
;;;  (at (and (1 2) (1 4)) cashier)  These two locations each get a cashier
;;;  (* 2 smoke fire)                2 random locs get both smoke and fire
;;;</PRE>

(defun parse-specs (env specs)
  "Place objects, defined by specs, in the environment."
  (for each spec in specs do
       (parse-spec env spec)))

(defun parse-spec (env spec)
  (case (op spec)
   (AT (parse-where env (arg1 spec) (rest (args spec))))
   (*  (for i = 1 to (parse-n (arg1 spec)) do
	 (parse-specs env (rest (args spec)))))
   (t  (parse-what env nil spec))))

(defun parse-where (env where whats)
  (cond
   ((eq where 'EDGE)    (let ((x-size (xy-x (grid-environment-size env)))
			      (y-size (xy-y (grid-environment-size env))))
			  (for i = 0 to (- x-size 1) do
			       (parse-whats env (@ i 0) whats)
			       (parse-whats env (@ i (- y-size 1)) whats))
			  (for i = 1 to (- y-size 2) do
			       (parse-whats env (@ 0 i) whats)
			       (parse-whats env (@ (- x-size 1) i) whats))))
   ((eq where 'ALL)     (dotimes (x (xy-x (grid-environment-size env)))
			  (dotimes (y (xy-y (grid-environment-size env)))
			    (when (free-loc? (@ x y) env)
			      (parse-whats env (@ x y) whats)))))
   ((eq where 'FREE?)   (parse-whats env (random-loc env :if 'free-loc?) whats))
   ((eq where 'START)   (parse-whats env (grid-environment-start env) whats))
   ((xy-p where)        (parse-whats env where whats))
   ((eq (op where) 'AND)(for each w in (args where) do 
			     (parse-where env w whats)))
   (t (warn "Unrecognized object spec ignored: ~A" `(at ,where ,@whats)))))

(defun parse-whats (env loc what-list)
  (for each what in what-list do
       (parse-what env loc what)))

(defun parse-what (env loc what)
  "Place the objects specified by WHAT-LIST at the given location
  The default location is START for an agent, random otherwise.
  The notation (P 0.5 what...) means 50% chance of placing each what,
  and (* n what...) means place n copies of each what."
  (case (op what)
    (* (for i = 1 to (parse-n (arg1 what)) do
	 (parse-whats env loc (rest (args what)))))
    (P (for each w in (rest (args what)) do
	    (when (< (random 1.0) (arg1 what))
	      (parse-what env loc w))))
    (t (let* ((object (if (object-p what) what
			(apply #'make (op what) (args what))))
	      (location (or loc (if (agent-p object)
				    (grid-environment-start env)
				  (random-loc env :if #'free-loc?)))))
	 (place-object object location env t)))))
    
(defun parse-n (n)
  (if (eq (op n) '+-)
      (round (+ (arg1 n) (random (float (arg2 n)))
		(- (random (float (arg2 n))))))
      n))

(defun make (type &rest args)
  "Make an instance of the specified type by calling make-TYPE."
  (apply (concat-symbol 'make- type) args))

(defun random-loc (env &key (if #'true) (tries 100))
  "Return a random loc, somewhere in the environment.
  The loc must satisfy the :IF predicate.  If it can't find such a location
  after a number of TRIES, it signals an error."
  (or (for i = 1 to tries do
	   (let ((loc (mapcar #'random (grid-environment-size env))))
	     (when (funcall if loc env) (RETURN loc))))
      (error "Can't find a location.")))

(defun free-loc? (loc env)
  "A location is free if there is no obstacle there and it is not the start."
  (and (not (find-object-if #'obstacle-p loc env))
       (not (equal loc (grid-environment-start env)))))

