(defun start-word-sort-key-1 (word)
  (let ((letters *pangram-letters-sorted*))
    (position (car (remove-if-not (lambda (c) (find c word))
				  letters))
	      letters)))
    
(defun start-word-sort-key-2 (word)
  (loop with seen = nil
	for c across word
	if (member c seen) count c
	else do (push c seen)))

(defun start-word-sort-pred-< (a b)
  (let ((a1 (start-word-sort-key-1 a)) (b1 (start-word-sort-key-1 b))
	(a2 (start-word-sort-key-2 a)) (b2 (start-word-sort-key-2 b)))
    (if (= a2 b2)
	(< a1 b1)
	(< a2 b2))))

(defun make-worthy-start-word-p (wr)
  (lambda (w)
    (let ((r (reverse w)))
      (or (has-word-ending r wr)
	  (string= r w)))))
