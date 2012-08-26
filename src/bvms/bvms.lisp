;; sigs
;; bvms (bit vector member summary)
;; Given a sequence of possible elements of a sequence,
;; return a procedure that when called with a sequence
;; of zero or more of those elements, returns a bit
;; vector having a one or zero, corresponding to found
;; or not found, for the element in the position from the
;; original sequence as being a member of the given 
;; sequence.
;; bvms:make-bvms-gen

(in-package :bvms)

(defun memoize (fn)
  (let ((cache (make-hash-table :test 'equalp)))
    (lambda (&rest args)
      (multiple-value-bind (value hit) (gethash args cache)
	(if hit
	    value
	    (setf (gethash args cache) (apply fn args)))))))

(defun make-bvms-gen (members)
  "Creates a procedure that returns a BVMS (bit vector member summary)
   generator for a given SEQUENCE based on MEMBERS."
  (lambda (sequence)
    (loop with bit-vector = 0
	  for item across sequence
	  for position = (position item members)
	  for byte = (byte 1 position)
	  do (setf (ldb byte bit-vector) 1)
	  finally (return bit-vector))))

(defmacro defbvms (name members &optional (memoize nil))
  `(let ((fn (make-bvms-gen ,members)))
     (setf (symbol-function ',name)
	   (if ,memoize
	       (memoize fn)
	       fn))
     ',name))

