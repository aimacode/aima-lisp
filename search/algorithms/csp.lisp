
;;;; Definition of CSPs (Constraint Satisfaction Problems).

(defstructure (CSP-problem (:include problem))
  "A Constraint Satisfaction Problem involves filling in values for variables.
  We will use a CSP-state structure to represent this."
  (forward-checking? nil)      ; should we filter domains?
  (legality-checking? nil)      ; should we check for legal values early?
  (variable-selector #'first)  ; what variable should we work on next?
  )

;;; All CSPs use integers as names for both variables and their values.
;;; Constraints on variables var1, var2 are represented by a table,
;;; indexed by var1, var2, with each entry a list of all allowable pairs
;;; of values for var1, var2.


(defstructure CSP-state
  unassigned                   ;; variables that have not been given values
  assigned                     ;; variables with known values
  constraint-fn                ;; checks allowed pairwise assignments
  modified                     ;; variable modified to make this state
  )

(defstruct (CSP-var (:type list))
  name domain value conflicts)

;;;; Generic Functions for CSP Problems

(defmethod goal-test ((problem csp-problem) node-or-state)
  "STATE is a goal if all variables are assigned legally."
  (let ((state (if (node-p node-or-state) (node-state node-or-state) 
		 node-or-state)))
    (and (null (CSP-state-unassigned state));; The state is legal
	 (CSP-legal-statep state))))

(defmethod successors  ((problem CSP-problem) s)
  (let ((unassigned (CSP-state-unassigned s))
	(assigned (CSP-state-assigned s))
	(constraint-fn (CSP-state-constraint-fn s)))
    (if unassigned
	(let* ((var (funcall (CSP-problem-variable-selector problem)
			     unassigned))
	       (name (CSP-var-name var))
	       (values (CSP-var-domain var)))
	  (mapcar
	   #'(lambda (value)
	       (cons (cons name value)
		     (make-CSP-state
		      :unassigned
		      (if (CSP-problem-forward-checking? problem)
			  (filter-domains name value 
					  (remove var unassigned :test #'eq)
					  constraint-fn)
			(remove var unassigned :test #'eq))
		      :assigned (let ((new (copy-CSP-var var)))
				  (setf (CSP-var-value new) value)
				  (cons new assigned))
		      :constraint-fn constraint-fn)))
	   (if (CSP-problem-legality-checking? problem)
	       (CSP-legal-values var values assigned constraint-fn)
	     values)))
      nil)))

;;;; Algorithms for Solving Constraint Satisfaction Problems

(defun csp-backtracking-search (problem &optional
					(queuing-fn #'enqueue-at-front))
  ;; There are two ways to implement a basic backtracking CSP search.
  ;; The first is to use the current DEPTH-FIRST-SEARCH function with
  ;; a successor function that generates only legal successors. The
  ;; second is to insert a consistency check, CSP-LEGAL-STATEP, before
  ;; the goal check, and avoid expanding inconsistent states.  This
  ;; second approach is implemented by this function.
  (let ((nodes (make-initial-queue problem queuing-fn))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (when (CSP-legal-statep (node-state node))
	    (if (goal-test problem node) (RETURN node))
	    (funcall queuing-fn nodes (expand node problem))))))


(defun csp-forward-checking-search (problem &optional
					    (queuing-fn #'enqueue-at-front))
  ;; Forward checking search adds a test to make sure the assignments
  ;; so far have not eliminated all the possible values for one of the
  ;; unassigned variables. Assumes that the problem definition uses
  ;; CSP-forward-checking-successors, which removes conflicting values from
  ;; the domains of the unassigned variables each time a variable is assigned.
  ;; Forward checking could also be implemented using depth-first search
  ;; and a successor function that drops any successor that has an empty
  ;; domain for some unassigned variable.
  (setf (csp-problem-forward-checking? problem) t)
  (let ((nodes (make-initial-queue problem queuing-fn))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (when (and (CSP-legal-statep (node-state node))
		     (not (CSP-empty-domainp (node-state node))))
	    (if (goal-test problem node) (RETURN node))
	    (funcall queuing-fn nodes (expand node problem))))))

;;;; Auxiliary Functions

(defmethod print-structure ((state csp-state) stream)
  (format stream "#<CSP ~A>" (CSP-state-assigned state)))

(defun CSP-legal-statep (s)
  (every #'(lambda (var1)
	     (every #'(lambda (var2)
			(funcall (CSP-state-constraint-fn s)
				 (CSP-var-name var1) (CSP-var-value var1)
				 (CSP-var-name var2) (CSP-var-value var2)))
		    (cdr (member var1 (CSP-state-assigned s) :test #'eq))))
	 (CSP-state-assigned s)))


(defun filter-domains (name value unassigned constraint-fn)
  (mapcar #'(lambda (var)
	      (let ((name2 (CSP-var-name var))
		    (domain (CSP-var-domain var)))
		(make-CSP-var :name name2
			      :domain (remove-if-not
				       #'(lambda (val2)
					   (funcall constraint-fn
						    name value name2 val2))
				       domain))))
	  unassigned))



(defun CSP-modifications (s &optional 
			    (variable-selector-fn #'random-conflicted-variable))
  ;; CSP-modifications is a successor function that assumes
  ;; the state is already complete but inconsistent, as in e.g.
  ;; min-conflicts-hill-climbing-search.
  (let* ((assigned (CSP-state-assigned s))
	 (constraint-fn (CSP-state-constraint-fn s))
	 (var (funcall variable-selector-fn assigned)))
    (if var
	(let ((name (CSP-var-name var))
	      (values (CSP-var-domain var)))
	  (mapcar #'(lambda (value)
		      (let ((s2 (copy-CSP-state s)))
			(modify-assignment s2 var name value 
					   assigned constraint-fn)
			(cons (cons name value) s2)))
		  values))
      nil)))




(defun modify-assignment (s var name new assigned constraint-fn
			  &aux (old (CSP-var-value var))
			       (var-copy (copy-CSP-var var)))
  ;; modify-assignment produces a new assignment in which var changes
  ;; its value to new. Need to update all the conflict counts.
  (setf (CSP-state-assigned s)
    (mapcar #'(lambda (var2)
	     (cond 
	      ((eq var var2)
	       (setf (CSP-var-value var-copy) new)
	       (setf (CSP-state-modified s) var-copy))
	      (t (let ((val2 (CSP-var-value var2))
		      (name2 (CSP-var-name var2))
		      (var2-copy (copy-CSP-var var2)))
		  (unless (funcall constraint-fn name old name2 val2)
			  (decf (CSP-var-conflicts var-copy))
			  (decf (CSP-var-conflicts var2-copy)))
		  (unless (funcall constraint-fn name new name2 val2)
			  (incf (CSP-var-conflicts var-copy))
			  (incf (CSP-var-conflicts var2-copy)))
		  var2-copy))))
	    assigned)))


;;(defun CSP-MCV-successors (s)
;;  (CSP-successors s #'most-constrained-variable t nil))

(defun most-constrained-variable (vars)
  (the-smallest #'(lambda (var) (length (CSP-var-domain var))) vars))

(defun random-conflicted-variable (vars)
  (let ((conflicted (remove-if-not #'plusp vars :key #'CSP-var-conflicts)))
    (if conflicted (random-element conflicted) nil)))

(defun min-conflicts-value (s &aux (v (CSP-state-modified s)))
  (if v (CSP-var-conflicts v) infinity))

(defun CSP-empty-domainp (s)
  (some #'(lambda (var) (null (CSP-var-domain var))) (CSP-state-unassigned s)))

(defun CSP-legal-values (name values assigned constraint-fn)
  (remove-if-not #'(lambda (value) 
		     (CSP-legal-assignmentp name value assigned constraint-fn))
		 values))

(defun CSP-legal-assignmentp (name value assigned constraint-fn)
  (every #'(lambda (var)
	     (funcall constraint-fn name value 
		                    (CSP-var-name var) (CSP-var-value var)))
	 assigned))

(defun CSP-explicit-check (name1 value1 name2 value2 constraints)
  (member (cons value1 value2) (aref constraints name1 name2) :test #'equal))

(defun CSP-random-completion (s)
  (dolist (var (CSP-state-unassigned s))
    (setf (CSP-var-value var) (random-element (CSP-var-domain var)))
    (push var (CSP-state-assigned s)))
  (setf (CSP-state-unassigned s) nil)
  (dolist (var (CSP-state-assigned s))
    (setf (CSP-var-conflicts var)
	  (CSP-conflicts var (CSP-state-assigned s) 
			 (CSP-state-constraint-fn s))))
  s)

(defun CSP-conflicts (var vars constraint-fn 
			  &aux (sum 0) (name (CSP-var-name var))
			               (value (CSP-var-value var)))
  (dolist (var2 vars sum)
    (unless (or (eq var var2)
		(funcall constraint-fn 
			 name value
			 (CSP-var-name var2) (CSP-var-value var2)))
      (incf sum))))

		     

