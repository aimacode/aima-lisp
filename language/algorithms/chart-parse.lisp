;;;; Chart Parser with Unification Augmentation

(defstructure grammar
  "A grammar for a chart parser has rules indexed by word and LHS."
  (lexicon nil)
  (rules nil)
  (start-symbol 'S)
  (categories-for (make-hash-table :test #'eq))
  (rewrites-for (make-hash-table :test #'eq))
  (unknown-word-cats '(noun verb adjective adverb)))

(defvar *grammar* nil 
  "The currently used grammar.  Defining a new grammar changes this, or you
  can set it yourself.")

(defun rule-lhs (rule) "The left hand side." (first rule))
(defun rule-rhs (rule) "The right-hand side." (nthcdr 2 rule))

(defstructure chart
  "A chart has a vector that holds the edges that end at vertex i." 
  ;; A more efficient implementation would store other things
  (ends-at #()))

(defstructure (edge)
  "An edge represents a dotted rule instance. In the edge [i, j, L -> F . R],
  i is the start, j is the end, L is the lhs, (F) is found, and (R) remains."
  ;; The FOUND slot is stored in reverse order, so you can just push on it.
  start end lhs found remains bindings)

;;;; Chart Parsing Algorithm

(defun chart-parse (words &optional (*grammar* *grammar*))
  "See if the string of words can be parsed by the grammar.  (See page 702.)"
  (let ((chart (make-chart :ends-at (make-array (+ 1 (length words)) 
						:initial-element nil))))
    (add-edge (edge 0 0 'S* nil (list (grammar-start-symbol *grammar*)))
	      chart 'initializer)
    (for v = 0 to (- (length words) 1) do
         (scanner v (elt words v) chart))
    chart))

(defun scanner (j word chart)
  "Add edges everywhere WORD is expected."
  (for each cat in (categories-for word *grammar*) do
       (dprint "scanner:" cat (elt (chart-ends-at chart) j))
       (when (member cat (elt (chart-ends-at chart) j) 
		     :test #'unify :key #'edge-expects)
         (add-edge (edge j (+ j 1) cat (list word) nil) chart 'scanner))))

(defun predictor (edge chart)
  "Add edges saying what we expect to see here."
  (for each rule in (rewrites-for (op (edge-expects edge)) *grammar*) do
       (add-edge (edge (edge-end edge) (edge-end edge) 
                       (rule-lhs rule) 
                       nil (rule-rhs rule))
                 chart 'predictor)))

(defun completer (edge chart)
  "Use this edge to extend any edges in the chart."
  (for each old-edge in (elt (chart-ends-at chart) (edge-start edge)) do
       (let ((b (unify (edge-lhs edge) (edge-expects old-edge)
		       (edge-bindings old-edge))))
	 (when b
	   (add-edge (edge (edge-start old-edge) (edge-end edge) 
			   (edge-lhs old-edge)
			   (cons edge (edge-found old-edge))
			   (rest (edge-remains old-edge))
			   b)
		     chart 'completer)))))

(defun add-edge (edge chart &optional reason)
  "Put edge into chart, and complete or predict as appropriate."
  (unless (member edge (elt (chart-ends-at chart) (edge-end edge))
		  :test #'edge-equal)
      (when (handle-augmentation *grammar* edge)
	(push edge (elt (chart-ends-at chart) (edge-end edge)))
	(dprint reason edge);; debugging output (as in Figure 23.4, [p 700])
	(if (complete? edge)
	    (completer edge chart)
	  (predictor edge chart)))))

;;;; Other Top-Level Functions

(defun chart-parses (words &optional (*grammar* *grammar*))
  "See if the string of words can be parsed by the grammar.  If it can, look 
  into the chart and pull out complete spanning strings."
  (mapcar #'edge->tree (spanning-edges (chart-parse words *grammar*))))

(defun meanings (words &optional (*grammar* *grammar*))
  "Parse words, then pick out the semantics of each parse.
  Assumes the semantics will be the last element of the LHS."
  (delete-duplicates
   (mapcar #'(lambda (edge) (last1 (mklist (edge-lhs edge))))
	  (spanning-edges (chart-parse words *grammar*)))
   :test #'equal))

;;;; Auxiliary Functions

(defun spanning-edges (chart)
  "Find the edges that span the chart and form the start symbol."
  (remove-if-not 
   #'(lambda (e) 
       (and (complete? e)
	    (eql (edge-start e) 0)
	    (eq (op (edge-lhs e)) (grammar-start-symbol *grammar*))))
   (elt (chart-ends-at chart) (- (length (chart-ends-at chart)) 1))))

(defun edge->tree (edge) 
  "Convert an edge into a parse tree by including its FOUND parts."
  (cond ((edge-p edge) 
	 (cons (edge-lhs edge) 
	       (mapcar #'edge->tree (reverse (edge-found edge)))))
        (t edge)))

(defun edge (start end lhs found remains &optional (bindings +no-bindings+))
  "Construct a new edge."
  (make-edge :start start :end end :lhs lhs :found found :remains remains
	     :bindings bindings))

(defun grammar (&rest args)
  "Take a list of rules, index them to form a grammar for chart-parse."
  (setf *grammar* (apply #'make-grammar args))
  (for each rule in (grammar-lexicon *grammar*) do
      (for each word in (rule-rhs rule) do
          ;; Rule [A -> word] means index A under categories-for word
          ;; Replace (A $w) with (A word)
          (let ((lhs (subst-bindings `(($w . ,word)) (rule-lhs rule))))
            (push lhs (gethash word (grammar-categories-for *grammar*))))))
  (for each rule in (grammar-rules *grammar*) do
      ;; Rule [A -> B C] indexed under rewrites for A
      (push rule (gethash (op (rule-lhs rule))
                          (grammar-rewrites-for *grammar*))))
  *grammar*)

(defun rewrites-for (lhs grammar)
  "Find the rules in grammar with LHS as the left hand side."
  (gethash (op lhs) (grammar-rewrites-for grammar)))

(defun categories-for (word grammar)
  "Find what categories this word can be.
  For unknown words, use the grammar's unknown-word-cats field"
  (or (gethash word (grammar-categories-for grammar))
      (subst word '$w (grammar-unknown-word-cats grammar))))

(defun edge-expects (edge)
  "What does the edge expect next in order to be extended?"
  (first (edge-remains edge)))

(defun lhs-op (edge) 
  "Left hand side of an edge's category" 
  (if (edge-p edge) (op (edge-lhs edge)) edge))

(defun complete? (edge) 
  "An edge is complete if it has no remaining constituents."
  (null (edge-remains edge)))

(defun edge-equal (edge1 edge2)
  "Are two edges the same, up to renaming of the parts with variables?"
  (and (eql (edge-start edge1) (edge-start edge2))
       (eql (edge-end edge1) (edge-end edge2))
       (eql (op (edge-lhs edge1)) (op (edge-lhs edge2)))
       (renaming? (edge-found edge1) (edge-found edge2))
       (renaming? (edge-remains edge1) (edge-remains edge2))))

(defmethod handle-augmentation ((grammar grammar) edge)
  "There are two things to do: (1) When we start a new edge, rename vars.
  (2) When an edge is complete, substitute the bindings into the lhs."
  (when (null (edge-found edge)) ;; (1) rename vars
    (let ((new (rename-variables (cons (edge-lhs edge) (edge-remains edge)))))
      (setf (edge-lhs edge) (first new)
            (edge-remains edge) (rest new))))
  (when (complete? edge) ;; (2) substitute bindings into lhs
    (setf (edge-lhs edge) 
	  (subst-bindings (edge-bindings edge) (edge-lhs edge))))
  (edge-bindings edge))

(defmethod print-structure ((e edge) stream) 
  (format stream "[~D, ~D, ~A ->~{ ~A~} .~{ ~A~}]"
          (edge-start e) (edge-end e) (lhs-op e) 
          (nreverse (mapcar #'lhs-op (edge-found e)))
	  (mapcar #'lhs-op (edge-remains e))))

