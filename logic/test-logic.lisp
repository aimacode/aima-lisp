;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/test.lisp

;;;; Testing Logical Inference

(deftest logic
  "Some simple examples in Propositional Logic"
  "First, just test the infix reader."
  ((logic "P=>Q <=> ~Q=>~P") '(<=> (=> P Q) (=> (not Q) (not P))))
  "Print a truth table, as on [p 169]."
  ((truth-table "(P | H) ^ ~H => P"))
  
  "Some simple examples"
  ((validity "P=>Q <=> ~Q=>~P") 'valid)
  ((validity "SillyQuestion") 'satisfiable)
  ((validity "~SillyQuestion") 'satisfiable)
  ((validity "ToBe or not ToBe") 'valid)
  ((validity "ToBe and not ToBe") 'unsatisfiable)
  ((validity "((S => W1|W2|W3|W4) ^ S ^ (~W1^~W2^~W3)) => W4") 'valid)
  ((validity "Ok ^ (Ok <=> ~W^~P) => ~W") 'valid)
  ((setf kb (make-prop-kb)))
  ((tell kb "S => W1|W2|W3|W4"))
  ((tell kb "S"))
  ((tell kb "~W1"))
  ((tell kb "~W2"))
  ((ask kb "W4") 'nil)
  ((tell kb "~W3"))
  ((ask kb "W4") 't)
  ((tell kb "Ok <=> ~W ^ ~P"))
  ((tell kb "Ok"))
  ((ask kb "W") 'nil)
  ((ask kb "~W") 't)
  ((tell kb "ToBe and ~ToBe"))
  ((ask kb "SillyQuestion") 't)
  
  "A look at Normal forms (conjunctive, implicative, and Horn)."
  ((->cnf '(<=> P Q))
   '(AND (OR P (NOT Q)) (OR (NOT P) Q)))
  ((->inf '(<=> P Q))
   '(AND (=> Q P) (=> P Q)))
  ((->horn '(<=> P Q))
   '(AND (=> Q P) (=> P Q)))
  ((->cnf '(=> (not P) R))
   '(OR R P))
  ((->inf '(=> (not P) R))
   '(=> TRUE (OR R P)))
  
  "Use the KB to solve the `Wumpus at [1,3]' problem [p 174-176]."
  "This builds a KB with 12 propositional symbols -- about the max."
  "you can do without starting to slow down."
  ((setq kb1 (make-prop-kb)))
  "The initial state of knowledge"
  ((tell kb1 "~S11 ^ ~S21 ^S12 ^ ~B11 ^ B21 ^ ~B12"))
  "Rules R1 through R4"
  ((tell kb1 "~S11 => ~W11 ^ ~W12 ^ ~W21"))
  ((tell kb1 "~S21 => ~W11 ^ ~W21 ^ ~W22 ^ ~W31"))
  ((tell kb1 "~S12 => ~W11 ^ ~W12 ^ ~W22 ^ ~W13"))
  ((tell kb1 "S12 => W13 | W12 | W22 | W11"))
  "Now the query -- this may take a while."
  ((ask kb1 "W13") *)
  
  "Now a quick demo of the Horn Logic backward chainer."
  ((setf kb2 (make-horn-kb)))
  "Now we define the Member predicate."
  ((tell kb2 "Member(x,Cons(x,y))"))
  ((tell kb2 "Member(x,rest) => Member(x,Cons(y,rest))"))
  ((ask-each kb2 "Member(x,Cons(1,Cons(2,Cons(3,Nil))))" #'print))
  ((ask-patterns kb2 "Member(x,Cons(1,Cons(2,Cons(3,Nil))))" "x") '(1 2 3))
  ((ask-pattern kb2 "Member(x,Cons(1,Cons(2,Cons(3,Nil)))) & x=2" "x") '2)
  ((ask-patterns kb2 "s = Cons(1,Cons(2,Nil))
                        & Member(x,s) & Member(y,s)" '($x $y))
   '((1 1) (1 2) (2 1) (2 2)))
 
  "A family relationships knowledge base and problem."
  ((tell kb2 '(Mother Gerda Peter)))
  ((tell kb2 '(Father Torsten Peter)))
  ((tell kb2 '(Father Peter Isabella)))
  ((tell kb2 '(Father Peter Juliet)))
  ((tell kb2 '(=> (mother $x $y) (parent $x $y))))
  ((tell kb2 '(=> (father $x $y) (parent $x $y))))
  ((tell kb2 '(=> (and (parent $g $p) (parent $p $c)) (grand-parent $g $c))))
  ((ask-patterns kb2 '(grand-parent $x $y)) 
   '((Grand-parent Gerda Isabella) (Grand-parent Gerda Juliet) 
     (Grand-parent Torsten Isabella) (Grand-parent Torsten Juliet)))
  
  "Now the 'Criminal' problem from [p 271-272]."
  ((setf kb3 (make-horn-kb)))
  ((tell kb3 "American(x) ^ Weapon(y) ^ Nation(z) ^ Hostile(z) ^ Sells(x,z,y)
		 => Criminal(x)"))
  ((tell kb3 "Owns(Nono,M1)"))
  ((tell kb3 "Missle(M1)"))
  ((tell kb3 "Owns(Nono,x) ^ Missle(x) => Sells(West,Nono,x)"))
  ((tell kb3 "Missle(x) => Weapon(x)"))
  ((tell kb3 "Enemy(x,America) => Hostile(x)"))
  ((tell kb3 "American(West)"))
  ((tell kb3 "Nation(Nono)"))
  ((tell kb3 "Enemy(Nono,America)"))
  ((tell kb3 "Nation(America)"))
  ((ask kb3 "Criminal(West)") 't)
  ((ask-pattern kb3 "Criminal(x)" "x") 'West)
  
  )
