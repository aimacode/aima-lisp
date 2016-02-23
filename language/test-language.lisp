
(deftest language 
  "Test the chart parser on some grammars."
  "First the simple E0 grammar from page 665."
  ((chart-parses '(I smell a stench) *E0*)
   '((S (NP (PRONOUN I)) 
        (VP (VP (VERB SMELL)) (NP (ARTICLE A) (NOUN STENCH))))))
  ((chart-parses '(the gold is in 2 2) *E0*)
   '((S (NP (ARTICLE THE) (NOUN GOLD)) 
        (VP (VP (VERB IS)) (PP (PREPOSITION IN) (NP (DIGIT 2) (DIGIT 2)))))))
  "Now the E1 grammar to show how pronoun case is handled."
  "It is grammatical to use 'I' as a subject, but not 'me'."
  ((chart-parses '(I shot the wumpus) *E1*)
   (renaming? * '((S ((NP SUBJECTIVE) ((PRONOUN SUBJECTIVE) I)) 
                     (VP (VERB SHOT) 
                         ((NP $CASE.10) (ARTICLE THE) (NOUN WUMPUS)))))))
   ((chart-parses '(Me shot the wumpus) *E1*)
    'NIL)
   "The E0 grammar allows anything (including 'me') as a subject:"
   ((chart-parses '(Me shot the wumpus) *E0*)
    '((S (NP (PRONOUN ME)) 
         (VP (VP (VERB SHOT)) (NP (ARTICLE THE) (NOUN WUMPUS))))))
   "Now for a longer sentence"
   ((chart-parses '(I see the wumpus in 2 3 and it is smelly ) *e1*)
    (renaming?
     *
     '((S (S ((NP SUBJECTIVE) ((PRONOUN SUBJECTIVE) I)) 
             (VP (VERB SEE) 
                 ((NP $CASE.218) ((NP $CASE.220) (ARTICLE THE) (NOUN WUMPUS)) 
                  (PP (PREPOSITION IN) ((NP $CASE.225) (DIGIT 2) (DIGIT 3)))))) 
          (CONJUNCTION AND) 
          (S ((NP $CASE.234) ((PRONOUN $CASE) IT)) 
             (VP (VERB IS) (ADJECTIVE SMELLY)))))))
   "An example from the simple arithmetic grammar."
   ((chart-parses '([ 1 + 2 ] * 3 0) *arithmetic-grammar*)
    '(((EXP (* (+ 1 2) (+ (* 10 3) 0))) 
       ((EXP (+ 1 2)) 
        ([ [) 
        ((EXP (+ 1 2)) 
         ((EXP 1) ((NUMBER 1) ((DIGIT 1) 1)))
         ((OPERATOR +) +)
         ((EXP 2) ((NUMBER 2) ((DIGIT 2) 2))))
        (] ]))
       ((OPERATOR *) *)
       ((EXP (+ (* 10 3) 0)) 
        ((NUMBER (+ (* 10 3) 0)) ((NUMBER 3) ((DIGIT 3) 3)) ((DIGIT 0) 0))))))
   "The function MEANINGS picks out just the semantics"
   ((meanings '([ 1 + 2 ] * 3 0) *arithmetic-grammar*)
    '((* (+ 1 2) (+ (* 10 3) 0))))
   "Note that strings can be ambiguous, yielding two or more parses."
   ((meanings '(1 + 2 * 3) *arithmetic-grammar*)
    '((* (+ 1 2) 3) (+ 1 (* 2 3))))
   ((chart-parses '(1 + 2 * 3) *arithmetic-grammar*)
    '(((EXP (* (+ 1 2) 3)) 
       ((EXP (+ 1 2)) ((EXP 1) ((NUMBER 1) ((DIGIT 1) 1)))
        ((OPERATOR +) +) ((EXP 2) ((NUMBER 2) ((DIGIT 2) 2))))
       ((OPERATOR *) *) ((EXP 3) ((NUMBER 3) ((DIGIT 3) 3))))
      ((EXP (+ 1 (* 2 3))) 
       ((EXP 1) ((NUMBER 1) ((DIGIT 1) 1))) 
       ((OPERATOR +) +) 
       ((EXP (* 2 3)) ((EXP 2) ((NUMBER 2) ((DIGIT 2) 2))) ((OPERATOR *) *) 
        ((EXP 3) ((NUMBER 3) ((DIGIT 3) 3)))))))
   ((chart-parses '(i shot the wumpus that stinks) *e2*)
    (renaming?
     *
     '(((S ((SHOT (THE $X.648 (AND (WUMPUS $X.648) (STINKS $X.648)))) I))
        ((NP I) ((PRONOUN I) I))
        ((VP (SHOT (THE $X.648 (AND (WUMPUS $X.648) (STINKS $X.648)))))
         ((VP SHOT) ((VERB SHOT) SHOT))
         ((NP (THE $X.648 (AND (WUMPUS $X.648) (STINKS $X.648))))
          ((NP (THE $X.655 (WUMPUS $X.655))) ((ARTICLE THE) THE)
           ((NOUN WUMPUS) WUMPUS))
          ((RELCLAUSE STINKS) (THAT THAT)
           ((VP STINKS) ((VERB STINKS) STINKS)))))))))
   ((meanings '(i shoot the wumpus that stinks and i grab the gold) *e2*)
    (renaming?
     *
     '((AND ((SHOOT (THE $X.746 (AND (WUMPUS $X.746) (STINKS $X.746)))) I)
            ((GRAB (THE $X.851 (GOLD $X.851))) I)))))

   )