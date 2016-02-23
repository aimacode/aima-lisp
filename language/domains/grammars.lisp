;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: language/grammars.lisp

;;;; Definition of Lexicons and Grammars: E0, E1, E2

(defparameter *E0*
  (grammar
   :lexicon
   '((Noun        -> stench breeze glitter nothing wumpus pit pits
	             gold north south east west)
     (Verb        -> is see smell shoot shot feel stinks go grab 
		     carry kill turn)
     (Adjective   -> right left east south back smelly)
     (Adverb      -> here there nearby ahead right left 
		     north south east west back)
     (Pronoun     -> me you I it)
     (Name        -> John Mary Boston Aristotle)
     (Article     -> the a an)
     (Preposition -> to in on near)
     (Conjunction -> and or but)
     (Digit       -> 0 1 2 3 4 5 6 7 8 9)
     (that        -> that))

   :rules
   '((S  -> NP VP)
     (S  -> S Conjunction S)
     (NP -> Pronoun) 
     (NP -> Noun) 
     (NP -> Article Noun) 
     (NP -> Digit Digit) 
     (NP -> NP PP) 
     (NP -> NP RelClause)
     (VP -> Verb) 
     (VP -> VP NP)
     (VP -> VP Adjective) 
     (VP -> VP PP) 
     (VP -> VP Adverb)
     (PP -> Preposition NP)
     (RelClause -> that VP)))
  "Lexicon and grammar for E<sub>0</sub> in Figures 22.5, 22.6, page 665.")

(defparameter *E1*
  (grammar
   :lexicon
   '((Noun      -> stench breeze glitter nothing wumpus pit pits
                   gold north south east west)
     (Verb      -> is see smell shoot shot feel stinks go grab 
                   carry kill turn)
     (Adjective -> right left east south back smelly)
     (Adverb    -> here there nearby ahead right left 
                   north south east west back)
     ((Pronoun subjective) -> I you he she)
     ((Pronoun objective)  -> me you him her)
     ((Pronoun $case) -> it)
     (Name      -> John Mary Boston Aristotle)
     (Article   -> the a an)
     (Preposition -> to in on near)
     (Conjunction -> and or but)
     (Digit       -> 0 1 2 3 4 5 6 7 8 9)
     (that      -> that))
   
   :rules
   '((S  -> (NP subjective) VP)
     (S  -> S Conjunction S)
     ((NP $case) -> (Pronoun $case)) 
     ((NP $case) -> Noun) 
     ((NP $case) -> Article Noun) 
     ((NP $case) -> Digit Digit) 
     ((NP $case) -> (NP $case) PP) 
     ((NP $case) -> (NP $case) RelClause)
     (VP -> Verb) 
     (VP -> Verb (NP objective))
     (VP -> Verb Adjective) 
     (VP -> Verb PP) 
     (VP -> Verb Adverb)
     (PP -> Preposition (NP objective))
     (RelClause -> that VP)))
  "Lexicon and grammar for E<sub>1</sub> in Figure 22.10, page 670.")

(defparameter *E2*
  (grammar
   :lexicon
   '(((Noun $w)        -> stench breeze glitter nothing wumpus pit pits
	                  gold north south east west)
     ((Verb $w)        -> is see smell shoot shot feel stinks go grab 
		          carry kill turn)
     ((Adjective $w)   -> right left east south back smelly)
     ((Adverb $w)      -> here there nearby ahead right left 
                          north south east west back)
     ((Pronoun $w)     -> me you I it)
     ((Name $w)        -> John Mary Boston Aristotle)
     ((Article $w)     -> the a an)
     ((Preposition $w) -> to in on near)
     ((Conjunction $w) -> and or but)
     ((Digit $w)       -> 0 1 2 3 4 5 6 7 8 9)
     (that             -> that))

   :rules
   '(((S ($rel $obj))  -> (NP $obj) (VP $rel))
     ((S ($conj $sem1 $sem2))  -> (S $sem1) (Conjunction $conj) (S $sem2))
     ((NP $sem) -> (Pronoun $sem))
     ((NP $sem) -> (Name $sem))
     ;; ?? Need nouns with no article, e.g. "dogs" is an NP
     ((NP ($q $x ($sem $x))) -> (Article $q) (Noun $sem))
     ((NP ($q $x (and $obj ($rel $x)))) -> (NP ($q $x $obj)) (PP $rel))
     ((NP ($q $x (and $obj ($rel $x)))) -> (NP ($q $x $obj)) (RelClause $rel))
     ((NP (@ $sem1 $sem2) -> (Digit $sem1) (Digit $sem2)))

     ;; VP rules for subcategorization
     ((VP $sem) -> (Verb $sem)) 
     ((VP ($rel $obj)) -> (VP $rel) (NP $obj))
     ((VP ($sem1 $sem2)) -> (VP $sem1) (Adjective $sem2)) 
     ((VP ($sem1 $sem2)) -> (VP $sem1) (PP $sem2))
     ;; VP rules for adjuncts
     ((VP (lambda $x (and ($sem1 $x) ($sem2 (event-var $sem1))))) ->
      (VP $sem1) (PP $sem2))
     ((VP (lambda $x (and ($sem1 $x) ($sem2 (event-var $sem1))))) ->
      (VP $sem1) (Adverb $sem2))

     ((RelClause $sem) -> that (VP $sem))
     ((PP (lambda $x ($rel $x $obj))) -> (Preposition $rel) (NP $obj))))
  "Lexicon and grammar for E<sub>2</sub> in Figure 22.19, page 680.")

;;;; Other grammars: Arithmetic, Trivial

(defparameter *arithmetic-grammar*
  (grammar
   :start-symbol  'Exp
   :rules
   '(((Exp ($op $sem1 $sem2)) -> (Exp $sem1) (Operator $op) (Exp $sem2))
     ((Exp $sem) -> [ (Exp $sem) ])
     ((Exp $sem) -> (Number $sem))
     ((Number $sem) -> (Digit $sem))
     ((Number  (+ (* 10 $sem1) $sem2)) -> (Number $sem1) (Digit $sem2)))

   :lexicon
   '(((Digit $w) -> 0 1 2 3 4 5 6 7 8 9)
     ((Operator $w) -> + - * /)
     ([ -> \( [)
     (] -> \) ])))
  "A grammar of arithmetic expressions, with semantics, from Figure 22.13, 
 page 673.")

(defparameter *figure23.4*
  (grammar
   :lexicon (grammar-lexicon *E0*)
   :rules
   '((S  -> NP VP)
     (NP -> Pronoun)
     (VP -> Verb)
     (VP -> VP NP)))
  "A grammar that, with debugging on, produces output similar to that
  on page 700, Figure 23.4.  The differences are: (1) Scanner does two
  steps in the book; here those steps are broken into Scanner and Completer. 
  (2) Some 'irrelevant' edges were ommitted from Figure 23.4")