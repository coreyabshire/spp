(defstruct word string lcount height length)

(defun make-swords (words)
  (loop with swords = (make-array (length words))
	for w in words
	for i from 0 below (length words)
	for lcount = (lcount w +alphabet+)
	for height = (loop for i across lcount maximize i)
	for length = (length w)
	for sword = (make-word :string w :lcount lcount :height height :length length)
	do (setf (aref swords i) sword)
	finally (return swords)))

;(defparameter +swords+ (make-swords +words+))

(defun sword-height-then-count (w1 w2)
  (if (equal (word-height w1) (word-height w2))
      (> (word-length w1) (word-length w2))
      (< (word-height w1) (word-height w2))))

;; removed because (map 'vector #'identity s) works just as well
(defun list->array (s)
  (make-array (length s) :initial-contents s))

(defun pick-random-words (n words)
  "Pick n random words."
  (loop with nwords = (length words)
	repeat n
	collect (word-string (nth (random nwords) words))))


; (defparameter +contains-words+ (group-contains-words +words+ +alphabet+))

;(defparameter +counts+ (sort (count-by +words+ +alphabet+ #'contains) #'< :key #'cdr))

; (find-p "jo" '("abc" "cba" "cab"))

(defgeneric new-starts-with (x))

(defmethod new-starts-with ((s string))
  (let ((n (length s)))
    (lambda (w)
      (let ((m (length w)))
	(if (> n m)
	    (string= (subseq s 0 m) w)
	    (string= (subseq w 0 n) s))))))

;(defparameter +jwords+ (remove-if-not (contains #\j) +words+))

;(defparameter +jwordsr+ (sort (mapcar #'reverse +jwords+) #'string<))

(defvar *short-words*
  (sort (map 'vector #'identity '("cde" "fgab" "bagf" "edc")) #'string<))

(defvar *short-words-r*
  (sort (map 'vector #'reverse *short-words*) #'string<))

(defun test-exceptions (n)
  (let ((v (vector 0 1 2)))
    (handler-case (aref v n) (t () 'error))))

(defun find-palindrome (word words rwords)
  (palindrome words rwords (list word) ()))

(defun try-some (n dict)
  "Try running the search randomly a few times."
  (loop for i from 0 below n
	for p = (palindrome dict (list (pick-one dict)) ())
	when p do (progn (format t "~a: ~a~%" i p)
			 (finish-output))))

(defun try-all (words-to-try all-words all-words-reverse)
  (loop for w across words-to-try
	for i from 1
	for p = (palindrome all-words all-words-reverse nil (list w) () "")
	when p do (progn (format t "~a: ~a~%" i p)
			 (finish-output))))

(defun try-more (n words)
  (loop for i from 0 below n
	for s = (reverse (pick-one words))
	for f = (new-starts-with s)
	for w = (mapcar #'reverse (remove-if-not f words))
	when w do (progn (format t "~a: ~a~%" s w) (finish-output))))

(defvar +lcounts-all+)

(setf +lcounts-all+ (loop for c across +alphabet+
			  for w = (remove-if-not (lambda (w) (find c w)) +words+)
			  collect (cons c (length w)) into counts
			  finally (return (sort counts #'< :key #'cdr))))

(defun count-by (words chars condition)
  (loop with a = (make-array (length +words+))
	for w in words
	do (loop for c across chars
		 when (funcall (funcall condition c) w)
		 do (incf (aref a (position c chars))))
	finally (return (loop for c across chars
			      for i from 0
			      collect (cons c (aref a i))))))

