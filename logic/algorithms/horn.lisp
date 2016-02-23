;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/horn.lisp

;;;; Logical Reasoning in Horn Clause Knowledge Bases

(defstructure horn-kb
  ;; Index all Horn sentences by the predicate on the right-hand side.
  ;; That is, both (=> P (Q x)) and (Q 3) would be indexed under Q.
  (table (make-hash-table :test #'eq)))

(defmethod tell ((kb horn-kb) sentence)
  "Add a sentence to a Horn knowledge base.  Warn if its not a Horn sentence."
  (for each clause in (conjuncts (->horn sentence)) do
       ;; Each clause should be of form (=> P (Q x)); add to hash for Q
       (setf (gethash (op (arg2 clause)) (horn-kb-table kb))
	     (nconc (gethash (op (arg2 clause)) (horn-kb-table kb))
		    (list clause)))))

(defmethod retract ((kb horn-kb) sentence)
  "Delete each conjunct of sentence from KB."
  (for each clause in (conjuncts (->horn sentence)) do
       ;; Each clause should be of form (=> P (Q x)); delete from hash for Q
       (deletef clause (gethash (op (arg2 clause)) (horn-kb-table kb))
		:test #'renaming?)))

(defmethod ask-each ((kb horn-kb) query fn)
  "Use backward chaining to decide if sentence is true."
  (back-chain-each kb (conjuncts (->cnf query)) +no-bindings+ fn))

(defun back-chain-each (kb goals bindings fn)
  "Solve the conjunction of goals by backward chaining.
  See [p 275], but notice that this implementation is different.
  It applies fn to each answer found, and handles composition differently."
  (cond ((eq bindings +fail+) +fail+)
	((null goals) (funcall fn bindings))
	(t (let ((goal (first goals)))
	     (case (op goal)
	       (FALSE +fail+)
	       (TRUE (back-chain-each kb (rest goals) bindings fn))
	       (= (back-chain-each kb (rest goals)
				   (unify (arg1 goal) (arg2 goal) bindings) 
				   fn))
	       (AND (back-chain-each kb (append (conjuncts goal) goals)
				     bindings fn))
	       (OR (for each disjunct in (disjuncts goal) do
                        (back-chain-each kb (cons disjunct goals)
                                         bindings fn)))
	       (NOT +fail+)		; Horn clause provers can't handle NOT
	       (t ;; Look at all the clauses that could conclude the goal.
		(for each clause in (gethash (op goal) (horn-kb-table kb)) do
		     (let ((new-clause (rename-variables clause)))
		       (back-chain-each
			kb
			(append (conjuncts (arg1 new-clause)) (rest goals))
			(unify goal (arg2 new-clause) bindings)
                        fn)))))))))

