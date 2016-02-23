;;;-*- Mode: Lisp;  -*-

;;;; Prefix to Infix Conversion

(defparameter *infix-ops* 
  '((([ list match ]) ({ elts match }) (|(| nil match |)|))
    ((*) (/))
    ((+) (-))
    ((<) (>) (<=) (>=) (=) (/=))
    ((not not unary) (~ not unary))
    ((and) (& and) (^ and))
    ((or) (\| or))
    ((=>))
    ((<=>))
    ((|,|)))
  "A list of lists of operators, highest precedence first.")

(defun ->prefix (infix)
  "Convert an infix expression to prefix."
  (when (stringp infix) (setf infix (string->infix infix)))
  ;; INFIX is a list of elements; each one is in prefix notation.
  ;; Keep reducing (most tightly bound first) until there is only one 
  ;; element left in the list.  Example: In two reductions we go:
  ;; (a + b * c) => (a + (* b c)) => ((+ a (* b c)))
  (loop 
    (when (not (length>1 infix)) (RETURN (first infix)))
    (setf infix (reduce-infix infix))))

(defun reduce-infix (infix)
  "Find the highest-precedence operator in INFIX and reduce accordingly."
  (dolist (ops *infix-ops* (error "Bad syntax for infix expression: ~S" infix))
    (let* ((pos (position-if #'(lambda (i) (assoc i ops)) infix
                             :from-end (eq (op-type (first ops)) 'MATCH)))
           (op (when pos (assoc (elt infix pos) ops))))
      (when pos
        (RETURN
         (case (op-type op)
           (MATCH (reduce-matching-op op pos infix))
           (UNARY (replace-subseq infix pos 2 
                                  (list (op-name op) 
                                        (elt infix (+ pos 1)))))
           (BINARY (replace-subseq infix (- pos 1) 3
                                   (list (op-name op)
                                         (elt infix (- pos 1)) 
                                         (elt infix (+ pos 1)))))))))))

(defun op-token (op) (first op))
(defun op-name (op) (or (second op) (first op)))
(defun op-type (op) (or (third op) 'BINARY))
(defun op-match (op) (fourth op))

(defun replace-subseq (sequence start length new)
  (nconc (subseq sequence 0 start) (list new)
         (subseq sequence (+ start length))))

(defun reduce-matching-op (op pos infix)
  "Find the matching op (paren or bracket) and reduce."
  ;; Note we don't worry about nested parens because we search :from-end
  (let* ((end (position (op-match op) infix :start pos))
         (len (+ 1 (- end pos)))
         (inside-parens (remove-commas (->prefix (subseq infix (+ pos 1) end)))))
    (cond ((not (eq (op-name op) '|(|)) ;; handle {a,b} or [a,b]
           (replace-subseq infix pos len 
                           (cons (op-name op) inside-parens))) ; {set}
          ((and (> pos 0)  ;; handle f(a,b)
                (function-symbol? (elt infix (- pos 1))))
           (handle-quantifiers
            (replace-subseq infix (- pos 1) (+ len 1)
                           (cons (elt infix (- pos 1)) inside-parens))))
          (t ;; handle (a + b)
           (assert (length=1 inside-parens))
           (replace-subseq infix pos len (first inside-parens))))))
                    
(defun remove-commas (exp)
  "Convert (|,| a b) to (a b)."
  (cond ((eq (op exp) '|,|) (nconc (remove-commas (arg1 exp) )
                                   (remove-commas (arg2 exp))))
        (t (list exp))))

(defun handle-quantifiers (exp)
  "Change (FORALL x y P) to (FORALL (x y) P)."
  (if (member (op exp) '(FORALL EXISTS))
    `(,(op exp) ,(butlast (rest exp)) ,(last1 exp))
    exp))

;;;; Tokenization: convert a string to a sequence of tokens
      
(defun string->infix (string &optional (start 0))
  "Convert a string to a list of tokens."
  (multiple-value-bind (token i) (parse-infix-token string start)
    (cond ((null token) nil)
          ((null i) (list token))
          (t (cons token (string->infix string i))))))

(defun parse-infix-token (string start)
  "Return the first token in string and the position after it, or nil."
    (let* ((i (position-if-not #'whitespace? string :start start))
           (ch (if i (char string i))))
      (cond ((null i) (values nil nil))
            ((find ch "+-~()[]{},") (values (intern (string ch)) (+ i 1)))
            ((find ch "0123456789") (parse-integer string :start i :junk-allowed t))
            ((symbol-char? ch) (parse-span string #'symbol-char? i))
            ((operator-char? ch) (parse-span string #'operator-char? i))
            (t (error "unexpected character: ~C" ch)))))

(defun parse-span (string pred i)
  (let ((j (position-if-not pred string :start i)))
    (values (make-logic-symbol (subseq string i j)) j)))

(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not forall exists) :test #'string-equal))
        ((lower-case-p (char string 0)) 
	 (concat-symbol "$" (string-upcase string)))
	((equal string "Nil") '|Nil|)
        (t (intern (string-upcase string)))))
            
(defun operator-char? (x) (find x "<=>&^|*/,"))

(defun symbol-char? (x) (or (alphanumericp x) (find x "$!?%")))

(defun function-symbol? (x) 
  (and (symbolp x) (not (member x '(and or not ||)))
       (alphanumericp (char (string x) 0))))

(defun whitespace? (ch)
  (find ch " 	
"))