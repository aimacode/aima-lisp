;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/test.lisp

;;;; Test Cases for Search

(deftest search
  "Test the code for Solving Problems by Searching"
  "Start with a trivial version of the missionaries and cannibals puzzle."
  ((setq p1 (make-cannibal-problem 
	     :initial-state (make-cannibal-state :m1 2 :c1 1))))
  "We search for a solution node:"
  ((setq result (breadth-first-search p1)) *)
  "We can get information out of that solution:"
  ((solution-actions result) *)
  ((solution-nodes result) *)
  "Or we can use SOLVE to print the results nicely. By default, SOLVE
  uses A*-search, but you can give it another algorithm as the second arg."
  ((solve p1) *)
  "For the full 3 missionary and 3 cannibal problem, breadth-first-search"
  "is very inefficient.  Better to use something that handles repeated states,"
  "like A*-search or no-duplicates-breadth-first-search:"
  ((solve (make-cannibal-problem) 'A*-search) *)
  ((solve (make-cannibal-problem) 'no-duplicates-breadth-first-search) *)

  "Here is how to get a problem-solving agent to find the solution,"
  "and then go ahead and execute the actions that comprise the solution."
  ((run-environment (problem->environment p1)))

  "Now we look at the route-finding domain."
  "First, solve the Arad-to-Bucharest problem with A*-search:"
  ((solve (make-romanian-problem :initial-state 'Arad :goal 'Bucharest)) *)
  "Now turn it around:"
  ((solve (make-romanian-problem :goal 'Arad :initial-state 'Bucharest)) *)
  "Let's just see the actions:"
  ((solution-actions (A*-search (make-romanian-problem)))
   '(Sibiu Rimnicu Pitesti Bucharest))
  "Now on a random map:"
  ((solve (make-route-finding-problem)))

  "Here's how to compare several algorithms."
  ((setq searchers '(A*-search no-cycles-depth-first-search
			       no-duplicates-breadth-first-search)))
  ((compare-search-algorithms #'make-route-finding-problem searchers))
  ((compare-search-algorithms #'make-romanian-problem searchers :n 1))
  ((compare-search-algorithms #'make-cannibal-problem
			      '(no-returns-breadth-first-search A*-search
				no-duplicates-breadth-first-search)
			      :n 1))
  ((compare-search-algorithms #'make-romanian-problem
			     '(tree-A*-search A*-search tree-IDA*-search)
			     :n 1))
  "We'll look at the iterative improvement algorithms on a harder map problem."
  ((setq searchers '(A*-search hill-climbing-search
			       simulated-annealing-search)))
  ((compare-search-algorithms #'(lambda () (make-romanian-problem :goal 'Iasi))
			      searchers :n 1))
  "Let's take a look at the 8-puzzle:"
  ((solve (make-8-puzzle-problem)) *)
  ((compare-search-algorithms 'make-8-puzzle-problem '(A*-search) :n 2))
  "And the path-planning problem among polygonal obstacles:"
  ((solve (make-path-planning-problem :scene *scene-4.17*)))
  "Now the 8-queens problem"
  ((solve (make-nqueens-problem) 'csp-backtracking-search) *)
  ((compare-search-algorithms
    'make-nqueens-problem
    '(csp-backtracking-search csp-forward-checking-search)
    :n 1))
  "Here is the Travelling Salesperson Problem (TSP)."
  ((solve (make-tsp-problem)))
  ((compare-search-algorithms 'make-tsp-problem
			     '(A*-search greedy-search uniform-cost-search)
			     :n 5))
  "Now we test the environments for 2-player and 3-player games:"
  ((run-game (make-ttt-game)))
  ((run-game (make-cognac-game :players '(X O @))))
  "Now we see that 2x2 tic-tac-toe is a win for the first player, X."
  ((run-game (make-ttt-game :n 2) 
	     :agents '(alpha-beta-ttt-agent alpha-beta-ttt-agent)))
  "In a full 3x3 game, alpha-beta search (playing O) often wins."
  ((run-game (make-ttt-game) :agents '(random-game-agent alpha-beta-ttt-agent)))
  )


