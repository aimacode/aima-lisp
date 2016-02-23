;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: tell-ask.lisp

;;;; Main Functions on KBs: Tell, Retract, Ask-Each, Ask, Ask-Pattern[s]

;;; First we define a very simple kind of knowledge base, literal-kb,
;;; that just stores a list of literal sentences.

(defstructure literal-kb
  "A knowledge base that just stores a set of literal sentences."
  (sentences '()))

;;; There are three generic functions that operate on knowledge bases, and
;;; that must be defined as methods for each type of knowledge base: TELL,
;;; RETRACT, and ASK-EACH.  Here we show the implementation for literal-kb;
;;; elsewhere you'll see implementations for propositional, Horn, and FOL KBs.

(defmethod tell ((kb literal-kb) sentence)
  "Add the sentence to the knowledge base."
  (pushnew sentence (literal-kb-sentences kb) :test #'equal))

(defmethod retract ((kb literal-kb) sentence)
  "Remove the sentence from the knowledge base."
  (deletef sentence (literal-kb-sentences kb) :test #'equal))

(defmethod ask-each ((kb literal-kb) query fn)
  "For each proof of query, call fn on the substitution that 
  the proof ends up with."
  (declare (special +no-bindings+))
  (for each s in (literal-kb-sentences kb) do
       (when (equal s query) (funcall fn +no-bindings+))))

;;; There are three other ASK functions, defined below, that are
;;; defined in terms of ASK-EACH.  These are defined once and for all
;;; here (not for each kind of KB)."

(defun ask (kb query)
  "Ask if query sentence is true; return t or nil."
  (ask-each kb (logic query)
            #'(lambda (s) (declare (ignore s)) (RETURN-FROM ASK t))))

(defun ask-pattern (kb query &optional (pattern query))
  "Ask if query sentence is true; if it is, substitute bindings into pattern."
  (ask-each kb (logic query)
            #'(lambda (s) (RETURN-FROM ASK-PATTERN 
                            (subst-bindings s (logic pattern))))))

(defun ask-patterns (kb query &optional (pattern query))
  "Find all proofs for query sentence, substitute bindings into pattern
  once for each proof.  Return a list of all substituted patterns."
  (let ((pat (logic pattern))
        (results nil))
    (ask-each kb (logic query) 
              #'(lambda (s) (push (subst-bindings s pat) results)))
    (nreverse results)))
