(deftest learning
  ((setq p *restaurant-boolean-problem*))
  ((setq e (learning-problem-examples p) 
         g (learning-problem-goals p)))
  ((setq h (decision-list-learning 4 p)))
  ((accuracy h #'dlpredict e g)
   (= * 1.0))
  ((setq p *restaurant-multivalued-problem*))
  ((setq e (learning-problem-examples p) 
         g (learning-problem-goals p)))
  ((setq h (decision-tree-learning p)))
  ((accuracy h #'dtpredict e g)
   (= * 1.0))
  ((setq p *majority-boolean-problem*))
  ((setq e (learning-problem-examples p) 
         a (learning-problem-attributes p) 
         g (learning-problem-goals p)))
  ((setq h (nn-learning p (make-perceptron 11 1) #'perceptron-update)))
  ((accuracy h #'(lambda (e1 h1) (nn-output e1 h1 a g)) e g)
   (> * 0.8))
  ((setq p *restaurant-real12-problem*))
  ((setq e (learning-problem-examples p) 
         a (learning-problem-attributes p) 
         g (learning-problem-goals p)))
  ((setq h (nn-learning p (make-connected-nn '(10 4 1)) #'backprop-update)))
  ((accuracy h #'(lambda (e1 h1) (nn-output e1 h1 a g)) e g #'rms-error)
   (> * 0.8))
  ((setq a (make-mdp-agent
	     :name 'LM :program (make-passive-lms-learner))))
  ((setq e (make-mdp-environment :mdp *4x3-passive-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs
  ((setq a (make-mdp-agent
	     :name 'TD :program (make-passive-td-learner))))
  ((setq e (make-mdp-environment :mdp *4x3-passive-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs
  ((setq a (make-mdp-agent :name 'MA 
	     :program (make-maximizing-adp-learner '(left right up down)))))
  ((setq e (make-mdp-environment :mdp *4x3-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs
  ((setq a (make-mdp-agent :name 'QI
	     :program (make-maximizing-qi-learner '(left right up down)))))
  ((setq e (make-mdp-environment :mdp *4x3-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs
  ((setq a (make-mdp-agent :name 'EA
	     :program (make-exploring-adp-learner '(left right up down)))))
  ((setq e (make-mdp-environment :mdp *4x3-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs
  ((setq a (make-mdp-agent :name 'TQ
	     :program (make-exploring-tdq-learner '(left right up down)))))
  ((setq e (make-mdp-environment :mdp *4x3-mdp* :agents (list a)
				 :epochs-left 2)))
  ((run-environment e))  ;;; for now just make sure it runs

  )
