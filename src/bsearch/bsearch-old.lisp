(defun my-starts-with (w s)
  (let ((m (length w))
	(n (length s))
	(start 0))
    (if (> m n)
	(string= (subseq w start n) s)
	(string= (subseq s start m) w))))

(defun bs-position-if (word sequence &optional (start 0) (end (- (length sequence) 1)))
  "The goal of this function is to find the position of the first word
in the given sequence that starts with the given word. The sequence is
expected to be sorted, allowing the function to utilize binary search
techniques to improve performance."
  (if (<= start end)
      (let ((mid (+ start (floor (- end start) 2))))
	(let ((s (aref sequence mid)))
	  (if (my-starts-with s word)
	      (if (= start mid)
		  mid
		  (or (bs-position-if word sequence start (- mid 1))
		      mid))
	      (if (string<= word s)
		  (bs-position-if word sequence start (- mid 1))
		  (bs-position-if word sequence (+ mid 1) end)))))))

(defun binary-search (item vector &optional (start 0) (end (1- (length vector))))
  (if (>= start end)
      (if (string= item (aref vector start))
	  start
	  nil)
      (let ((middle (floor (+ start end) 2)))
	(let ((middle-item (aref vector middle)))
	  (cond ((string< item middle-item) (binary-search item vector start (- middle 1)))
		((string> item middle-item) (binary-search item vector (+ middle 1) end))
		(t middle))))))
							   
(defun bs-compare (a b)
  (let ((m (mismatch a b)))
    (if (and (not (null m)) (> m 0))
	(string>= (subseq a m) (subseq b m))
	(string>= a b))))

(defun bs (word sequence &optional (start 0))
  (bs-position-if word sequence start (length sequence)))

(defun bs-range (word words reversed)
  (let ((start (bs word words)))
    (if start
	(loop for i = start then (bs word words (+ i 1)) while i
	      for w = (aref words i)
	      collect (if reversed (reverse w) w)))))


