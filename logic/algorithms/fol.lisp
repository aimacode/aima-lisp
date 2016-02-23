;;;; First Order Logic (FOL) Tell, Retract, and Ask-Each

(defstruct fol-kb
  ;;; A FOL (First-Order Logic) KB stores clauses. 
  ;;; Access to the KB is via POSSIBLE-RESOLVERS, which takes a
  ;;; literal (e.g. (not D), or B), and returns all the clauses that
  ;;; contain the literal.  We also keep a list of temporary clauses, 
  ;;; added to the KB during a proof and removed at the end. Internally,
  ;;; clauses are in minimal-cnf format, which is CNF without the and/or.
  ;;; So (and (or P Q) (or R (not S))) becomes ((P Q) (R (not S)))
  (positive-clauses (make-hash-table :test #'eq))
  (negative-clauses (make-hash-table :test #'eq))
  (temp-added nil))

(defmethod tell ((kb fol-kb) sentence)
  "Add a sentence to a FOL knowledge base."
  (for each clause in (->minimal-cnf sentence) do
      (tell-minimal-cnf-clause kb clause)))

(defmethod retract ((kb fol-kb) sentence)
  "Delete each conjunct of sentence from KB."
  (retract-minimal-cnf-clauses kb (->minimal-cnf sentence)))

(defmethod ask-each ((kb fol-kb) query fn)
  "Use resolution to decide if sentence is true."
  (prove-by-refutation kb (->minimal-cnf `(not ,query)) fn))

;;;; FOL Knowledge Base Utility Functions

(defun possible-resolvers (kb literal)
  "Find clauses that might resolve with a clause containing literal."
  (if (eq (op literal) 'not)
      (gethash (op (arg1 literal)) (fol-kb-negative-clauses kb))
    (gethash (op literal) (fol-kb-positive-clauses kb))))

(defun tell-minimal-cnf-clause (kb clause)
  ;; We don't add tautologies like "P | ~P".
  ;; It would be good to eliminate subsumed clauses like
  ;; Eq(1,1) when Eq(x,x) is already in the kb.
  ;; Currently we don't check for that.
  (unless (tautology? clause)
      (for each literal in clause do
	   (if (eq (op literal) 'not)
	       (push clause (gethash (op (arg1 literal)) 
				     (fol-kb-negative-clauses kb)))
	     (push clause (gethash (op literal) 
				   (fol-kb-positive-clauses kb)))))))

(defun retract-minimal-cnf-clauses (kb clauses)
  "Remove the minimal-cnf clauses from the KB."
  (for each clause in clauses do
       (for each literal in clause do
	    (if (eq (op literal) 'not)
		(deletef clause 
			 (gethash (op (arg1 literal)) 
				  (fol-kb-negative-clauses kb)))
	      (deletef clause (gethash (op literal) 
				       (fol-kb-positive-clauses kb)))))))

(defun ->minimal-cnf (sentence)
  "Convert a logical sentence to minimal CNF (no and/or connectives)."
  ;; E.g., (and (or P (not Q) R) S) becomes ((P (not Q) R) (S))
  ;; Everything internal in the FOL module uses minimal-cnf
  ;; Only tell, retract, and ask-* use the regular logical form.
  (mapcar #'disjuncts (conjuncts (->cnf sentence))))

(defun undo-temp-changes (kb)
  "Undo the changes that were temporarilly made to KB."
  (retract-minimal-cnf-clauses kb (fol-kb-temp-added kb))
  (setf (fol-kb-temp-added kb) nil))

(defun tautology? (clause)
  "Is clause a tautology (something that is always true)?"
  (some #'(lambda (literal)
	    (and (eq (op literal) 'not)
		 (member (arg1 literal) clause :test #'equal)))
	clause))

;;;; Functions for Resolution Refutation Theorem Proving

(defun prove-by-refutation (kb sos fn)
  "Try to prove that ~SOS is true (given KB) by resolution refutation."
  ;; Call FN on every substitution that leads to a proof.
  ;; Similar to OTTER [p. 311], the KB plays the role of the usable
  ;; (background) axioms, and SOS (set of support) is formed by the
  ;; negation of the query.  Uses set of support heuristic and uses
  ;; shorter clauses first (which is a generalization of the unit
  ;; preference strategy).  Filters out tautologies.
  (setf sos (sort sos #'< :key #'length))
  (undo-temp-changes kb)
  (let (clause)
    (loop 
	(when (null sos) (RETURN nil))
        ;; Move clause from SOS to the usable KB
        (setf clause (pop sos))
        (tell-minimal-cnf-clause kb clause)
        (push clause (fol-kb-temp-added kb))
        ;; Process everything that resolves with CLAUSE
        (for each literal in clause do
            (for each r in (possible-resolvers kb literal) do
                (let ((b (unify ??? literal)))
                  (when b
	            (setf sos (insert clause sos #'< :key #'length))
                    (case (length clause)
                      (0 (funcall fn b)) ;; refutation found!!
                      ; should look for unit refutation if length is 1
                      ))))))))

(defun resolve (literal clause)
  "Resolve a single literal against a clause"
  )

(defun insert (item list pred &key (key #'identity))
  (merge 'list (list item) list pred :key key))



