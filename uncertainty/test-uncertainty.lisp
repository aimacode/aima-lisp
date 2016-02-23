(deftest uncertainty
  "Test code for reasoning with uncertainty.  Currently, just MDPs."
  "Given the MDP (Markov Decision Process) for the 4x3 grid from Ch.s 17, 20,"
  "create an agent, and an environment corresponding to the MDP."
  ((setq agent (make-mdp-agent :mdp *4x3-mdp*
			       :algorithm 'value-iteration-policy)))
  ((setq env (make-mdp-environment :mdp *4x3-mdp* 
				   :agents (list agent))))
  "Now run the agent in the environment."
  "If all goes well, we get to the (4 3) terminal square."
  ((run-environment env))
  )
