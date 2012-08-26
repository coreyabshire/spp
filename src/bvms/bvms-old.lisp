(defun compute-word-sig (word)
  (loop with i = 0
	for c across word
	for p = (position c *letters*)
	for b = (byte 1 p)
	do (setf (ldb b i) 1)
	finally (return i)))

(defun word-sig (word)
  (let ((cached (gethash word *word-sig-table*)))
    (if cached
	cached
	(setf (gethash word *word-sig-table*)
	      (loop with i = 0
		    for c across word
		    for p = (position c *letters*)
		    for b = (byte 1 p)
		    do (setf (ldb b i) 1)
		    finally (return i))))))

(defparameter *goal-sig* (word-sig *letters*))
(defparameter *goal-sig-len* (logcount *goal-sig*))

(defun format-sig (sig)
  (format nil "~26,'0B" sig))

(defun invert-sig (sig)
  (logxor sig *goal-sig*))

(defun unique-sig-counts (words)
  (loop with table = (make-hash-table)
	for word across words
	for key = (word-sig word)
	do (incf (gethash key table 0))
	finally (return table)))

(defun split-sig (sig)
  (loop for i from 0 below 26
	if (logbitp i sig)
	collect (ash 1 i)))

