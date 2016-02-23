;;; File: binary-tree.lisp -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;;;  The following definitions implement binary search trees.

;;;  They are not balanced as yet.  Currently, they all order their
;;;  elements by #'<, and test for identity of elements by #'eq.


(defstruct search-tree-node
  "node for binary search tree"
  value        ;; list of objects with equal key
  num-elements ;; size of the value set
  key          ;; f-cost of the a-star-nodes
  parent       ;; parent of search-tree-node
  leftson      ;; direction of search-tree-nodes with lesser f-cost
  rightson     ;; direction of search-tree-nodes with greater f-cost
  )


 
(defun make-search-tree (root-elem root-key &aux root)
  "return dummy header for binary search tree, with initial
  element root-elem whose key is root-key."
  (setq root
	(make-search-tree-node
	  :value nil
	  :parent nil
	  :rightson nil
	  :leftson (make-search-tree-node
		     :value (list root-elem)
		     :num-elements 1
		     :key root-key
		     :leftson nil :rightson nil)))
  (setf (search-tree-node-parent
	  (search-tree-node-leftson root)) root)
  root)



(defun create-sorted-tree (list-of-elems key-fun &aux root-elem root)
  "return binary search tree containing list-of-elems ordered according
  tp key-fun"
  (if (null list-of-elems)
      nil
      (progn
	(setq root-elem (nth (random (length list-of-elems)) list-of-elems))
	(setq list-of-elems (remove root-elem list-of-elems :test #'eq))
	(setq root (make-search-tree root-elem
				     (funcall key-fun root-elem)))
	(dolist (elem list-of-elems)
	  (insert-element elem root (funcall key-fun elem)))
	root)))



(defun empty-tree (root)
  "Predicate of search trees; return t iff empty."
  (null (search-tree-node-leftson root)))



(defun leftmost (tree-node &aux next)
  "return leftmost descendant of tree-node"
  ;; used by pop-least-element and inorder-successor
  (loop (if (null (setq next (search-tree-node-leftson tree-node)))
	    (return tree-node)
	    (setq tree-node next))))



(defun rightmost (header &aux next tree-node)
  "return rightmost descendant of header"
  ;; used by pop-largest-element
  ;; recall that root of tree is leftson of header, which is a dummy
  (setq tree-node (search-tree-node-leftson header))
  (loop (if (null (setq next (search-tree-node-rightson tree-node)))
	    (return tree-node)
	    (setq tree-node next))))


 
(defun pop-least-element (header)
  "return least element of binary search tree; delete from tree as side-effect"
  ;; Note value slots of search-tree-nodes are lists of a-star-nodes, all of
  ;; which have same f-cost = key slot of search-tree-node.  This function
  ;; arbitrarily returns first element of list with smallest f-cost,
  ;; then deletes it from the list.  If it was the last element of the list
  ;; for the node with smallest key, that node is deleted from the search
  ;; tree.  (That's why we have a pointer to the node's parent).
  ;; Node with smallest f-cost is leftmost descendant of header.
  (let* ( (place (leftmost header))
	 (result (pop (search-tree-node-value place))) )
      (decf (search-tree-node-num-elements place))
      (when (null (search-tree-node-value place))
	(when (search-tree-node-rightson place)
	  (setf (search-tree-node-parent
		  (search-tree-node-rightson place))
		(search-tree-node-parent place)))
	(setf (search-tree-node-leftson
	        (search-tree-node-parent place))
	      (search-tree-node-rightson place)))
      result))




(defun pop-largest-element (header)
  "return largest element of binary search tree; delete from tree as side-effect"
  ;; Note value slots of search-tree-nodes are lists of a-star-nodes, all of
  ;; which have same  key slot of search-tree-node.  This function
  ;; arbitrarily returns first element of list with largest key
  ;; then deletes it from the list.  If it was the last element of the list
  ;; for the node with largest key, that node is deleted from the search
  ;; tree. We need to take special account of the case when the largest element
  ;; is the last element in the root node of the search-tree.  In this case, it
  ;; will be in the leftson of the dummy header.  In all other cases,
  ;; it will be in the rightson of its parent.
  (let* ( (place (rightmost header)) 
	 (result (pop (search-tree-node-value place))) )
      (decf (search-tree-node-num-elements place))      
      (when (null (search-tree-node-value place))
	(cond ( (eq place (search-tree-node-leftson header))
	       (setf (search-tree-node-leftson header)
		     (search-tree-node-leftson place)) )
	      (t (when (search-tree-node-leftson place)
		   (setf (search-tree-node-parent
			   (search-tree-node-leftson place))
			 (search-tree-node-parent place)))
		 (setf (search-tree-node-rightson
			 (search-tree-node-parent place))
		       (search-tree-node-leftson place)))))
      result))




(defun least-key (header)
  "return least key of binary search tree; no side effects"
  (search-tree-node-key (leftmost header)))


(defun largest-key (header)
  "return least key of binary search tree; no side effects"
  (search-tree-node-key (rightmost header)))



(defun insert-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "insert new element at proper place in binary search tree"
  ;; See Reingold and Hansen, Data Structures, sect. 7.2.
  ;; When called initially, parent will be the header, hence go left.
  ;; Element is an a-star-node.  If tree node with key = f-cost of
  ;; element already exists, just push element onto list in that
  ;; node's value slot.  Else have to make new tree node.
  (loop (cond ( (null (setq place (funcall direction parent)))
	       (let ( (new-node (make-search-tree-node
				  :value (list element) :num-elements 1
				  :parent parent :key key
				  :leftson nil :rightson nil)) )
		 (if (eq direction #'search-tree-node-leftson)
		     (setf (search-tree-node-leftson parent) new-node)
		     (setf (search-tree-node-rightson parent) new-node)))
	       (return t))
	      ( (= key (search-tree-node-key place))
	       (push element (search-tree-node-value place))
	       (incf (search-tree-node-num-elements place))
	       (return t))
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson) )
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))




(defun randomized-insert-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "insert new element at proper place in binary search tree -- break
   ties randomly"
  ;; This is just like the above, except that elements with equal keys
  ;; are shuffled randomly.  Not a "perfect shuffle", but the point is
  ;; just to randomize whenever  an arbitrary choice is to be made.

  (loop (cond ( (null (setq place (funcall direction parent)))
	       (let ( (new-node (make-search-tree-node
				  :value (list element) :num-elements 1
				  :parent parent :key key
				  :leftson nil :rightson nil)) )
		 (if (eq direction #'search-tree-node-leftson)
		     (setf (search-tree-node-leftson parent) new-node)
		     (setf (search-tree-node-rightson parent) new-node)))
	       (return t))
	      ( (= key (search-tree-node-key place))
	       (setf (search-tree-node-value place)
		     (randomized-push element (search-tree-node-value place)))
	       (incf (search-tree-node-num-elements place))	       
	       (return t))
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson) )
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))




(defun randomized-push (element list)
  "return list with element destructively inserted at random into list"
  (let ((n (random (+ 1 (length list)))) )
    (cond ((= 0 n)
	   (cons element list))
	  (t (push element (cdr (nthcdr (- n 1) list)))
	     list))))




(defun find-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "return t if element is int tree"
  (loop (cond ( (null (setq place (funcall direction parent)))
		  (return nil) )
		 ( (= key (search-tree-node-key place))
		  (return (find element (search-tree-node-value place)
				:test #'eq)) ) 
		 ( (< key (search-tree-node-key place))
		  (setq parent place)
		  (setq direction #'search-tree-node-leftson) )
		 (t (setq parent place)
		    (setq direction #'search-tree-node-rightson)))))





(defun delete-element (element parent key &optional (error-p t)
		       &aux (direction #'search-tree-node-leftson)
		       place)
  "delete element from binary search tree"
  ;; When called initially, parent will be the header.
  ;; Have to search for node containing element, using key, also
  ;; keep track of parent of node.  Delete element from list for
  ;; node;  if it's the last element on that list, delete node from
  ;; binary tree.  See Reingold and Hansen, Data Structures, pp. 301, 309.
  ;; if error-p is t, signals error if element not found;  else just
  ;; returns t if element found, nil otherwise.
  (loop (setq place (funcall direction parent))
	(cond ( (null place) (if error-p
				 (error "delete-element: element not found") 
				 (return nil)) )
	      ( (= key (search-tree-node-key place))
	       (cond ( (find element (search-tree-node-value place) :test #'eq)
		      ;; In this case we've found the right binary
		      ;; search-tree node, so we should delete the
		      ;; element from the list of nodes 
		      (setf (search-tree-node-value place)
			    (remove element (search-tree-node-value place)
				    :test #'eq))
		      (decf (search-tree-node-num-elements place))
		      (when (null (search-tree-node-value place))
			;; If we've deleted the last element, we
			;; should delete the node from the binary search tree.
			(cond ( (null (search-tree-node-leftson place))
			       ;; If place has no leftson sub-tree, replace it
			       ;; by its right sub-tree.
			       (when (search-tree-node-rightson place)
				 (setf (search-tree-node-parent
					 (search-tree-node-rightson place))
				       parent))
			       (if (eq direction #'search-tree-node-leftson)
				   (setf (search-tree-node-leftson parent)
					 (search-tree-node-rightson place))
				   (setf (search-tree-node-rightson parent)
					 (search-tree-node-rightson place))) )
			      ( (null (search-tree-node-rightson place) )
			       ;; Else if place has no right sub-tree,
			       ;; replace it by its left sub-tree.
			       (when (search-tree-node-leftson place)
				 (setf (search-tree-node-parent
					 (search-tree-node-leftson place))
				       parent))
			       (if (eq direction #'search-tree-node-leftson)
				   (setf (search-tree-node-leftson parent)
					 (search-tree-node-leftson place))
				   (setf (search-tree-node-rightson parent)
					 (search-tree-node-leftson place))) )
			      (t ;; Else find the "inorder-successor" of
			       ;; place,  which must have nil leftson.
			       ;; Let it replace place, making its left
			       ;; sub-tree be place's current left
			       ;; sub-tree, and replace it by its own
			       ;; right sub-tree. (For details, see
			       ;; Reingold & Hansen, Data Structures, p. 301.)
			       (let ( (next (inorder-successor place)) )
				 (setf (search-tree-node-leftson next)
				       (search-tree-node-leftson place))
				 (setf (search-tree-node-parent
					 (search-tree-node-leftson next))
				       next)
				 (if (eq direction #'search-tree-node-leftson)
				     (setf (search-tree-node-leftson
					    parent) next) 
				     (setf (search-tree-node-rightson parent)
					   next))
				 (unless (eq next (search-tree-node-rightson
						    place))
				   (setf (search-tree-node-leftson
					   (search-tree-node-parent next))
					 (search-tree-node-rightson next))
				   (when (search-tree-node-rightson next)
				     (setf (search-tree-node-parent
					     (search-tree-node-rightson next))
					   (search-tree-node-parent next)))
				   (setf (search-tree-node-rightson next)
					 (search-tree-node-rightson
					   place))
				   (setf (search-tree-node-parent
					   (search-tree-node-rightson next))
					 next))
				 (setf (search-tree-node-parent next)
				       (search-tree-node-parent place))))))
		      (return t))
		     (t (if error-p
			    (error "delete-element:  element not found") 
			    (return nil)))) )
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson))
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))





(defun inorder-successor (tree-node)
  "return inorder-successor of tree-node assuming it has a right son"
  ;; this is used by function delete-element when deleting a node from
  ;; the binary search tree.  See Reingold and Hansen, pp. 301, 309.
  ;; The inorder-successor is the leftmost descendant of the rightson.
  (leftmost (search-tree-node-rightson tree-node)))



(defun list-elements (parent &aux child)
  "return list of elements in tree"
  (append (when (setq child (search-tree-node-leftson parent))
            (list-elements child))
          (search-tree-node-value parent)
          (when (setq child (search-tree-node-rightson parent))
            (list-elements child))))
