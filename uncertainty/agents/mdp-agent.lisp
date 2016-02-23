;;;; A simple policy agent for Markov decision processes (MDPs).

(defun make-mdp-agent (&key body name mdp program
                            (algorithm 'value-iteration-policy))
  "An MDP agent constructs a policy from the MDP once, and then uses that
  policy repeatedly to take action.  The ALGORITHM keyword specifies the
  algorithm that is used to create the policy; don't confuse it with the
  PROGRAM keyword, which decides what actions to take."
  (new-mdp-agent 
   :body body :name name
   :program (or program
                (let ((policy nil))
                  #'(lambda (percept)
                      (when (null policy)
                        (setf policy (funcall algorithm mdp)))
                      (policy-choice (mdp-percept-state percept) policy))))))
		 
(defstructure (mdp-agent (:include agent) (:constructor new-mdp-agent))
  (total-reward 0))

