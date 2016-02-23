;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/queue.lisp

;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front, to the back, or ordered by some numeric score.
;;; This is done with the following enqueing functions, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-AT-FRONT - elements are a list
;;;   ENQUEUE-AT-END   - elements are a list, with a pointer to end
;;;   ENQUEUE-BY-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.

;;; The heap implementation is taken from "Introduction to Algorithms" by
;;; Cormen, Lieserson & Rivest [CL&R], Chapter 7.  We could certainly speed
;;; up the constant factors of this implementation.  It is meant to be clear
;;; and simple and O(log n), but not super efficient.  Consider a Fibonacci
;;; heap [Page 420 CL&R] if you really have large queues to deal with.

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

;;;; Basic Operations on Queues

(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

;;;; The Three Enqueing Functions

(defun enqueue-at-front (q items)
  "Add a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q))))

(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (cond ((null items) nil)
	((or (null (q-last q)) (null (q-elements q)))
	 (setf (q-last q) (last items)
	       (q-elements q) (nconc (q-elements q) items)))
	(t (setf (cdr (q-last q)) items
		 (q-last q) (last items)))))

(defun enqueue-by-priority (q items key)
  "Insert the items by priority according to the key function."
  ;; First make sure the queue is in a consistent state
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (for each item in items do
       (heap-insert (q-elements q) item key)))

;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

;; These could be made inline

(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, move heap[i] down where it belongs.
  [Page 143 CL&R]."
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest valued) item off the heap. [Page 150 CL&R]."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Put an item into a heap. [Page 150 CL&R]."
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
      do (setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(defun heap-sort (numbers &key (key #'identity))
  "Return a sorted list, with elements that are < according to key first."
  ;; Mostly for testing the heap implementation
  ;; There are more efficient ways of sorting (even of heap-sorting)
  (let ((heap (make-heap))
	(result nil))
    (for each n in numbers do (heap-insert heap n key))
    (while (> (length heap) 0) do (push (heap-extract-min heap key) result))
    (nreverse result)))
