(in-package :words)

(defun group-contains-words (words letters)
  (loop for c across letters
	for subwords = (remove-if-not (contains c) words)
	for asarray = (map 'vector #'identity subwords)
	for sorted = (sort asarray #'< :key 'height)
	do (progn (format t "doing letter ~a~%" c) (finish-output))
	collect sorted into final
	finally (return (map 'vector #'identity final))))
	
(defun lcount (word letters)
  "Count of the letters of the word."
  (loop with n = (length letters)
	with v = (make-array n :element-type 'fixnum)
	for i from 0 below n
	for l = (char letters i)
	do (setf (aref v i) (count l word))
	finally (return v)))

(defun pangramp (words)
  "Is the given sequence of words a pangram?"
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz")
	(s (apply #'concatenate 'string words)))
    (every (lambda (c) (find c s)) alphabet)))

(defun palindromep (words)
  "Is the given sequence of words a palindrome?"
  (let ((s (apply #'concatenate 'string words)))
    (string= s (reverse s))))

(defun find-shortest-palindromic-pangram (words)
  (loop for w1 in words
	do (loop for w2 in words
		 for w = (list w1 w2)
		 if (palindromep w)
		 do (progn (format t "~a~%" w)
			   (finish-output)))))

(defun first-char (w)
  (char w 0))

(defun last-char (w)
  (char w (- (length w) 1)))

(defun contains (c)
  (lambda (w) (find c w)))

(defun contains-string (s)
  (lambda (w) (search s w)))

(defun left (w n)
  (subseq w 0 n))

(defgeneric starts-with (x))

(defmethod starts-with ((c character))
  (lambda (w) (equal c (first-char w))))

(defmethod starts-with ((s string))
  (let ((n (length s)))
    (lambda (w) (and (<= n (length w))
		     (equal (subseq w 0 n) s)))))

(defgeneric ends-with (c))

(defmethod ends-with ((c character))
  (lambda (w) (equal c (last-char w))))

(defmethod ends-with ((s string))
  (let ((n (length s)))
    (lambda (w)
      (let ((m (length w)))
	(and (<= n m)
	     (equal (subseq w (- m n)) s))))))

(defun height (w)
  (loop for c across w maximize (count c w)))

(defun has-height (n)
  (lambda (w) (equal n (height w))))

(defun ncartesians (n cs)
  (cond ((equal n 0) (list ""))
	(t (loop for w in (ncartesians (- n 1) cs)
		 nconc (loop for c across cs
			     collect (format nil "~a~a" c w))))))

(defun candidates (words counts path)
  (sort (remove-if-not (contains (car (car counts))) words) #'<
	:key (lambda (w) (height (apply #'concatenate 'string w path)))))

(defun remaining (counts word)
  (remove-if (lambda (c) (find c word))
	     counts :key #'car))

(defun find-all (words letters path counts)
  (format t "~a, ~a~%" (car counts) path)
  (finish-output)
  (cond ((null counts) (format t "~a~%" path))
	(t (loop with candidates = (candidates words counts path)
		 for word in candidates
		 do (find-all words letters (cons word path) (remaining counts word))))))

(defun find-p (word words)
  (loop with r = (reverse word)
	for w in words
	when (string= r w)
	collect (list word w)))

(defun permutations (letters length)
  (if (<= length 0)
      (list "")
      (loop for p in (permutations letters (- length 1))
	    nconc (loop for c across letters for s = (string c)
			collect (concatenate 'string s p)))))

(defun permutations-list (elements length)
  (if (<= length 0)
      (list ())
      (loop for p in (permutations-list elements (- length 1))
	    nconc (loop for e in elements
			collect (cons e p)))))
			
(defvar *tiny-words*)

(defvar *seed*)

(defun diff-strings (ls rs)
  (let ((ln (length ls))
	(rn (length rs)))
    (if (>= ln rn)
	(concatenate 'string "|" (subseq ls rn))
	(concatenate 'string (subseq rs 0 (- rn ln)) "|"))))

(defun diff-lists (left right)
  (diff-strings (apply #'concatenate 'string left)
		(apply #'concatenate 'string right)))

