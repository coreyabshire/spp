(in-package :palindrome)

(defclass palindrome ()
  ((lhs :initarg :lhs :accessor lhs)
   (rhs :initarg :rhs :accessor rhs)))

(defgeneric lhs-string (p))

(defmethod lhs-string ((p palindrome))
  (apply #'concatenate 'string (lhs p)))

(defgeneric rhs-string (p))

(defmethod rhs-string ((p palindrome))
  (apply #'concatenate 'string (rhs p)))

(defgeneric lhs-length (p))

(defmethod lhs-length (p)
  (length (lhs-string p)))

(defgeneric rhs-length (p))

(defmethod rhs-length (p)
  (length (rhs-string p)))

(defgeneric pal-string (p))

(defmethod pal-string ((p palindrome))
  (concatenate 'string (lhs-string p) (rhs-string p)))

(defgeneric pal-length (p))

(defmethod pal-length ((p palindrome))
  (length (pal-string p)))

(defgeneric repeats (p))

(defmethod repeats ((p palindrome))
  (count-repeats (pal-string p)))

(defgeneric append-lhs (p s))

(defmethod append-lhs ((p palindrome) s)
  (make-instance 'palindrome
		 :lhs (append (lhs p) (list s))
		 :rhs (append (rhs p))))

(defgeneric append-rhs (p s))

(defmethod append-rhs ((p palindrome) s)
  (make-instance 'palindrome
		 :lhs (append (lhs p))
		 :rhs (append (list s) (rhs p))))

(defmethod print-object ((object palindrome) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":lhs ~a :rhs ~a :repeats ~d"
	    (lhs object)
	    (rhs object)
	    (repeats object))))

(defgeneric pal-format (p))

(defmethod pal-format ((p palindrome))
  (format nil "~{~a~^ ~} ~{~a~^ ~}" (lhs p) (rhs p)))

(defgeneric pal-remainder (x))

(defmethod pal-remainder ((p palindrome))
  (if (> (lhs-length p) (rhs-length p))
      (subseq (lhs-string p) (rhs-length p))
      (subseq (rhs-string p) 0 (- (rhs-length p) (lhs-length p)))))

(defgeneric lhs-heavy-p (p))

(defmethod lhs-heavy-p ((p palindrome))
  (> (lhs-length p) (rhs-length p)))

(defgeneric dead-end-p (p dict))

(defmethod dead-end-p ((p palindrome) dict)
  (let ((rem (pal-remainder p)))
    (if (lhs-heavy-p p)
	(has-word-ending (reverse rem) dict)
	(has-word-starting (reverse rem) dict))))

(defun count-splits (dict)
  (loop with h = (make-hash-table :test #'equal)
	for word across (all-words dict)
	do (loop for i from 0 upto (length word)
		 do (incf (gethash (subseq word 0 i) h 0)))
	finally (return h)))

(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys of hash-table
	collect key))

(defun page-hash (h n)
  (loop with b = ()
	for k in (sort (hash-table-keys h) #'string<)
	for v = (gethash k h)
	do (push (cons k v) b)
	when (> (length b) n)
	do (progn (format t "~{~a~%~}" (reverse b))
		  (setf b ())
		  (read-line))))


  