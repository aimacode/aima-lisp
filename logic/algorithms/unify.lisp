;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/unify.lisp

;;;; Unification and Substitutions (aka Binding Lists)

;;; This code is borrowed from "Paradigms of AI Programming: Case Studies
;;; in Common Lisp", by Peter Norvig, published by Morgan Kaufmann, 1992.
;;; The complete code from that book is available for ftp at mkp.com in
;;; the directory "pub/Norvig".  Note that it uses the term "bindings"
;;; rather than "substitution" or "theta".  The meaning is the same.

;;;; Constants

(defconstant +fail+ nil "Indicates unification failure")

(defconstant +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

;;;; Top Level Functions

(defun unify (x y &optional (bindings +no-bindings+))
  "See if x and y match with given bindings.  If they do,
  return a binding list that would make them equal [p 303]."
  (cond ((eq bindings +fail+) +fail+)
        ((eql x y) bindings)
        ((variable? x) (unify-var x y bindings))
        ((variable? y) (unify-var y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t +fail+)))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (make-binding var (new-variable var)))
                  (variables-in x))
          x))

;;;; Auxiliary Functions

(defun unify-var (var x bindings)
  "Unify var with x, using (and maybe extending) bindings [p 303]."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in? var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))

(defun variable? (x)
  "Is x a variable (a symbol starting with $)?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\$)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
        (if (eq bindings +no-bindings+)
            nil
            bindings)))

(defun occurs-in? (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-in? var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in? var (first x) bindings)
                       (occurs-in? var (rest x) bindings)))
        (t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings +fail+) +fail+)
        ((eq bindings +no-bindings+) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable? exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (pushnew tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defvar *new-variable-counter* 0)

(defun new-variable (var)
  "Create a new variable.  Assumes user never types variables of form $X.9"
  (concat-symbol (if (variable? var) "" "$")
                 var "." (incf *new-variable-counter*)))

