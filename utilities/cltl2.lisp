;;; File: cltl2.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Compatibility package for 'Common Lisp the Language: 2nd edition'

;;; Functions and macros in CLtL2 that are not in the first edition of
;;; the book, and thus not in some old implementations of Common Lisp.

#+Allegro ;; Allow us to create missing functions in Allegro 
(when (fboundp 'package-definition-lock)
  (setf (package-definition-lock (find-package "COMMON-LISP")) nil))

(define-if-undefined
  
(defmacro with-simple-restart (restart &rest body)
  "Like PROGN, except provides control over restarts if there is an error."
  (declare (ignore restart))
  `(progn ,@body))

(defmacro destructuring-bind (lambda-list list &body body)
  "Bind the variables in lambda-list to the result list and execute body."
  ;; This implementation does not do the defmacro extensions,
  ;; Except that it does handle a trailing dot: (x y . z)
  (cond ((null lambda-list)
	 `(progn ,@body))
	((not (symbolp list))
	 (let ((var (gensym)))
	   `(let ((,var ,list))
	      (destructuring-bind ,lambda-list ,var ,@body))))
	((symbolp lambda-list)
	 `(let ((,lambda-list ,list)) ,@body))
	((atom lambda-list)
	 (error "Can't bind ~A to a value." lambda-list))
	((member (first lambda-list) '(&rest &optional &key &aux))
	 `(apply #'(lambda ,lambda-list ,@body) ,list))
	(t `(destructuring-bind ,(first lambda-list) (first ,list)
	      (destructuring-bind ,(rest lambda-list) (rest ,list)
		,@body)))))

  ) ; end define-if-undefined

;;;; Mini Implementation of CLOS

;;; If you don't have CLOS (the Common Lisp Object System) installed,
;;; then this defines a simple version of DEFMETHOD which only
;;; dispatches on the first argument, and works for structures (and
;;; some other types) but not classes.  Note that you can still do
;;; (single) inheritance with structures using the :include option.
;;; To properly inform DEFMETHOD of the inheritance tree, you should
;;; use DEFSTRUCTURE rather than DEFSTRUCT.  This has the added
;;; benefit of allowing you to write PRINT-STRUCTURE methods rather
;;; than :print-function functions, if you like (they will be
;;; inherited properly, and they don't have the silly DEPTH argument).

(defmacro defstructure (type-and-args &rest slots)
  "This is just like DEFSTRUCT, except it keeps track of :include types, for
  the benefit of METHOD-FOR, and it makes printing go through PRINT-STRUCTURE."
  (if (atom type-and-args) (setf type-and-args (list type-and-args)))
  (let* ((type (first type-and-args))
	 (args (rest type-and-args))
	 (supertype (or (second (assoc ':include args)) 'structure))
	 (print-fn (if (null (assoc ':print-function args))
		       '((:print-function (lambda (x s d)
					    (declare (ignore d))
					    (print-structure x s)))))))
    `(progn (setf (get ',type ':supertype) ',supertype)
	    (defstruct (,type ,@print-fn ,@args) ,@slots))))

(defmethod print-structure ((structure t) stream)
  "Print a structure.  You can specialize this function.
  It will be called to print anything defined with DEFSTRUCTURE."
  (format stream "#<a ~A>" (type-of structure)))

(eval-when (compile eval)
  (when (macro-function 'defmethod) 
    (pushnew :clos *features*)))

#-CLOS
(progn ;; when you don't have CLOS, use this simple version ...

(defmacro defmethod (name ((var class) &rest other-args) &rest body)
  "This version of DEFMETHOD is like the CLOS version, except it only
  dispatches on the first argument, and it only handles structures and
  some built-in types, not classes."
  `(setf (get ',name :generic) (ensure-generic-function ',name)
	 (get ',name ',class) #'(lambda (,var . ,other-args) . ,body)))

(defun ensure-generic-function (name)
  "Define NAME to be a generic function."
  (unless (eq (symbol-function name) (get name :generic))
    (setf (symbol-function name)
	  #'(lambda (var &rest args)
	      (labels ((call-next-method ()
		         (call-method-for name (supertype (type-of var))
					  var args)))
		(call-method-for name (type-of var) var args))))))
  
(defun supertype (type)
  "Find the most specific supertype of this type."
  (cond ((eq type t) nil)
	((get type :supertype))
	(t 'atom)))

(defun call-method-for (name type var args)
  "Find the method for this type, following :supertype links if needed."
  (let ((m (get name type)))
    (cond (m (apply m var args))
	  ((eq type nil) (error "Can't find method ~A for ~A." name var))
	  (t (call-method-for name (supertype type) var args)))))

;; Construct a small part of the built-in type hierarchy
(mapc 
 #'(lambda (pair) (setf (get (first pair) :supertype) (second pair)))
 '((null list) (cons list) (list t) (atom t) (keyword symbol) (null symbol)
   (fixnum integer) (bignum integer) (integer rational) (ratio rational) 
   (rational real) (float real) (real number) (complex number) 
   (string vector) (bit-vector vector) (vector array) (error condition)))

) ; end when you don't have CLOS ...
 
