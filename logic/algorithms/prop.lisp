;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File logic/prop.lisp

;;;; Propositional Logic

(defstructure prop-kb
  "A simple KB implementation that builds a big conjoined sentence."
  ;; The sentence slot will be, e.g., (and P (not Q) R (or S T) ...)
  (sentence (make-exp 'and)))

(defstructure truth-table
  symbols				; The propositional symbols
  sentences				; Sentences that head the columns
  rows					; Lists of t or nil truth values
  )

;;;; Tell, Ask, and Retract

(defmethod tell ((kb prop-kb) sentence)
  "Add a sentence to a propositional knowledge base."
  (push (logic sentence) (args (prop-kb-sentence kb)))
  t)

(defmethod ask-each ((kb prop-kb) query fn)
  "Ask a propositional knowledge base if the query is entailed by the kb."
  (when (eq (validity (make-exp '=> (prop-kb-sentence kb) (logic query)))
	    'valid)
    (funcall fn +no-bindings+)))

(defmethod retract ((kb prop-kb) sentence)
  "Remove a sentence from a knowledge base."
  ;; This only retracts sentences that were explicitly told to the kb.
  (deletef sentence (args (prop-kb-sentence kb)) :test #'equal)
  t)

;;;; Other Useful Top-Level Functions

(defun validity (sentence)
  "Return either VALID, SATISFIABLE or UNSATISFIABLE."
  (let* ((table (build-truth-table (logic sentence) :short t))
	 (rows (truth-table-rows table)))
    (cond ((every #'last1 rows) 'valid)
	  ((some #'last1 rows) 'satisfiable)
	  (t 'unsatisfiable))))

(defun truth-table (sentence)
  "Build and print a truth table for this sentence, with columns for all the
  propositions and all the non-trivial component sentences.  Iff the sentence
  is valid, the last column will have all T's.
  Example: (truth-table '(<=> P (not (not P))))."
  (print-truth-table (build-truth-table (logic sentence))))

;;;; Auxiliary Functions

(defun eval-truth (sentence &optional interpretation)
  "Evaluate the truth of the sentence under an interpretation.
  The interpretation is a list of (proposition . truth-value) pairs,
  where a truth-value is t or nil, e.g., ((P . t) (Q . nil)).
  It is an error if there are any propositional symbols in the sentence
  that are not given a value in the interpretation."
  (cond (interpretation (eval-truth (sublis interpretation sentence) nil))
	((eq sentence 'true) t)
	((eq sentence 'false) nil)
	((atom sentence) (error "No interpretation for ~A." sentence))
	(t (case (op sentence)
	     (or  (some #'eval-truth (args sentence)))
	     (and (every #'eval-truth (args sentence)))
	     (not (not (eval-truth (arg1 sentence))))
	     (=>  (or (eval-truth (arg2 sentence))
		      (not (eval-truth (arg1 sentence)))))
	     (<=> (eq (eval-truth (arg1 sentence))
		      (eval-truth (arg2 sentence))))
	     (otherwise (error "Unknown connective ~A in ~A"
			       (op sentence) sentence))))))

;; Note: a more efficient implementation of interpretations would be
;; a sequence of n propositional symbols and a number from 0 to (2^n)-1.
;; Symbol i is true iff bit i in the number is 1.

;;;; Truth Tables

(defun build-truth-table (sentence &key short)
  "Build a truth table whose last column is the sentence.  If SHORT is true,
  then that is the only column. If SHORT is false, all the sub-sentences
  are also included as columns (except constants)."
  (let* ((symbols (prop-symbols-in sentence))
	 (sentences (if short
			(list sentence)
			(append symbols (complex-sentences-in sentence)))))
    (make-truth-table :symbols symbols
		      :sentences sentences
		      :rows (compute-truth-entries symbols sentences))))

(defun print-truth-table (table &optional (stream t))
  "Print a truth table."
  (let* ((headers (mapcar #'sentence-output-form
			  (truth-table-sentences table)))
	 (width (+ (* 2 (length headers))
		   (sum headers #'length))))
    ;; Each sentence is printed as a column header, surrounded by 2 spaces
    (print-dashes width stream t)
    (format stream "~{ ~A ~}~%" headers)
    (print-dashes width stream t)
    (dolist (row (truth-table-rows table))
      (mapcar #'(lambda (entry header)
		  (print-centered (if entry "T" "F")
				  (+ 2 (length header))
				  stream))
	      row
	      headers)
      (format stream "~%"))
    (print-dashes width stream t)))

(defun compute-truth-entries (symbols sentences)
  "Compute the truth of each sentence under all interpretations of symbols."
  (mapcar #'(lambda (interpretation)
	      (mapcar #'(lambda (sentence)
			  (eval-truth sentence interpretation))
		      sentences))
	  (all-truth-interpretations symbols)))

(defun all-truth-interpretations (symbols)
  "Return all 2^n interpretations for a list of n symbols."
  (if (null symbols)
      (list nil)
      (let ((symbol1 (first symbols)))
	(mapcan #'(lambda (sub-rest)
		    `(((,symbol1 . false) . ,sub-rest)
		      ((,symbol1 . true) . ,sub-rest)))
		(all-truth-interpretations (rest symbols))))))
      
(defun prop-symbols-in (sentence)
  "Return a list of all the propositional symbols in sentence."
  (cond ((member sentence '(true false)) nil)
	((atom sentence) (list sentence))
	(t (delete-duplicates (mapcan #'prop-symbols-in (args sentence))
			      :from-end t))))

(defun complex-sentences-in (sentence)
  "Return a list of all non-atom sub-sentences of sentence."
  (cond ((atom sentence) nil)
	(t (delete-duplicates
	    (nconc (mapcan #'complex-sentences-in (args sentence))
		   (list sentence))))))

(defun sentence-output-form (sentence)
  "Convert a prefix sentence back into an infix notation with brief operators."
  (format nil "~{~A~^ ~}"
	  (mklist (sublis '((and . "^") (not . "~") (or . "|"))
			  (prefix->infix sentence)))))