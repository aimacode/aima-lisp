;;; -*- Mode: Lisp; -*- Author: Peter Norvig

;;;; A grammar for the wumpus world

(defparameter *E1*
  (grammar
   :lexicon
   '((Noun      -> stench breeze glitter nothing wumpus pit pits
                   gold north south east west)
     (Verb      -> is am are see smell shoot feel stinks go grab release 
                   carry kill turn)
     (Adjective -> right left east south back smelly)
     (Adverb    -> here there nearby ahead right left 
                   north south east west back)
     ((Pronoun subjective) -> I you he she it) ;; change here
     ((Pronoun objective)  -> me you him her it) ;; change here
     (Name      -> John Mary Boston Aristotle)
     (Article   -> the a an)
     (Preposition -> from to at in on near)
     (Conjunction -> and or but)
     (Digit       -> 0 1 2 3 4 5 6 7 8 9)
     (that      -> that))
   
   :rules
   '((S  -> (NP subjective) VP) ;; changes start here
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
     (PP -> Preposition (NP objective)) ;; changes end here
     (RelClause -> that VP)))
  "Lexicon and grammar for E<sub>1</sub> in Figure 22.10, page 670.")

	will, did
	a, the
	--
	yes, no, maybe, ok, huh

S -> Question | Command  | Report | Acknowledgement | S -- S

Question -> Aux NP VP | Be NP VP-args

Command -> "you" VP

Report -> "I" VP

NP -> Pronoun | {Article} Noun 

VP -> {Aux} Verb VP-args

VP-args -> {NP} {PP} {Adverb}

PP -> Prep NP

;;; Terminals

Acknowledgement -> "yes" | "no" | "ok" | "huh"

Verb -> Aux | "shoot" | ...

Aux -> Be | "will" | "did"

Be -> "is" | "am" | "are"

Adverb -> "here" | ...
