; Find the shortest pangram.
;
; Given a list of words and a list of letters,
; find the shortest combination of words such 
; that all the letters are represented at least
; once.
;

(defstruct (pangram-problem (:include problem (initial-state (make-pangram-state))))
  words
  word-table
  cost-table)

(defvar *pangram-words*)
(defvar *pangram-word-table*)
(defvar *pangram-cost-table*)
(defvar *pangram-letters-sorted*)

(defun init-pangram-data (&optional (letters *letters*) (filespec *filespec*))
  (let ((words (read-words filespec))
	(word-table (make-hash-table)))
    (let ((letters-sorted (letters-by-rarity words letters)))
      (loop for c across letters do (setf (gethash c word-table) ()))
      (loop for w in words do
	    (loop for c across letters if (find c w) do
		  (push w (gethash c word-table))))
      (let ((cost-table (letter-cost-table word-table letters)))
	(setf *pangram-words* words
	      *pangram-word-table* word-table
	      *pangram-cost-table* cost-table
	      *pangram-letters-sorted* letters-sorted))))
  nil)

(defstruct pangram-state words (length 0) seen (need (loop for c across "jxzaop" collect c)))

(defun make-initial-pangram-problem ()
  (make-pangram-problem :initial-state (make-pangram-state :need *pangram-letters-sorted*)
			:words *pangram-words*
			:word-table *pangram-word-table*
			:cost-table *pangram-cost-table*))

(defvar *pangram-solution-table* (make-hash-table :test #'equal))

(defun solve-pangram (need)
  ;(format t "solve subproblem: ~a~%" need)
  ;(finish-output)
  (let ((need-sorted (remove-if-not (lambda (c) (member c need)) *pangram-letters-sorted*)))
    (let ((solution (gethash need-sorted *pangram-solution-table*)))
      (cond (solution (format t "cache hit: ~a (size: ~a)~%" solution (hash-table-count *pangram-solution-table*))
		      solution)
	    (t (let ((problem (make-pangram-problem :initial-state (make-pangram-state :need need-sorted)
						    :words *pangram-words*
						    :word-table *pangram-word-table*
						    :cost-table *pangram-cost-table*)))
		 (let ((solution (solve problem 'tree-sma)))
		   ;(format t "here i am: ~a~%" need-sorted)
		   (if solution
		       (progn ;(format t "found solution: ~a~%" solution) (finish-output)
			      (setf (gethash need-sorted *pangram-solution-table*)
				    (node-state solution))
			      (node-state solution))
		       (progn (format t "failed to find solution!~%") (finish-output))))))))))

(defun letters-by-rarity (words letters)
  (loop with h = (make-hash-table)
	for w in words do
	(loop for c across letters
	      if (find c w)
	      do (incf (gethash c h 0)))
	finally (return (loop for k being the hash-keys of h
			      collect k into r
			      finally (return (sort r #'< :key (lambda (c) (gethash c h 0))))))))

(defun contains (word letters)
  (some (lambda (c) (find c word)) letters))

(defmethod successors ((problem pangram-problem) state)
  ;(format t "~a: " state) (finish-output)
  (let ((result (if (pangram-state-words state)
		    (let ((substate (solve-pangram (pangram-state-need state))))
		      ;(format t "here~%")
		      (list (cons (pangram-state-words substate)
				  (make-pangram-state :words (append (pangram-state-words substate) (pangram-state-words state))
						      :length (+ (pangram-state-length substate) (pangram-state-length state))
						      :seen (append (pangram-state-need state) (pangram-state-seen state))
						      :need ()))))
		    (loop for word in (gethash (car (pangram-state-need state)) (pangram-problem-word-table problem))
			  collect (cons word (make-pangram-state :words (cons word (pangram-state-words state))
								 :length (+ (length word) (pangram-state-length state))
								 :seen (new-seen (pangram-state-seen state) word)
								 :need (new-need (pangram-state-need state) word)))))))
    ;(format t "~a~%" (length result))
    ;(finish-output)
    result))

(defmethod goal-test ((problem pangram-problem) state)
  (null (pangram-state-need state)))

(defun count-extra (word seen)
  (loop for c across word
	if (member c seen) count c))

(defmethod h-cost ((problem pangram-problem) state)
  (if (and state (pangram-state-need state))
      (let ((cache (gethash (pangram-state-need state) *pangram-solution-table*)))
	(if cache
	    (pangram-state-length cache)
	    (length (pangram-state-need state))))
      0))

(defun unique-letters (word)
  (loop with seen = ()
	for c across word
	do (pushnew c seen)
	finally (return (length seen))))

(defun unique-letter-p (word)
  (= (length word) (unique-letters word)))
  
(defun letter-cost-table (word-table letters)
  (loop with h = (make-hash-table)
	for c across letters
	do (setf (gethash c h)
		 (loop for w in (gethash c word-table)
		       minimize (length w)))
	finally (return h)))

(defmethod edge-cost ((problem pangram-problem) node action state)
  (cond ((consp action) (loop for word in action sum (length word)))
	(t (length action))))
	
(defun n! (n)
  (if (<= n 1)
      1
      (loop for i from n downto 1
	    for r = n then (* r i)
	    finally (return r))))

(defun ncr (n r)
  (/ (n! n)
     (* (n! r)
	(n! (- n r)))))

(defun unwords (words)
  (loop with table = (make-hash-table :test #'equal)
	for word in words
	for unword = (sort word #'char<)
	if (gethash unword table) do (incf (gethash unword table))
	else do (setf (gethash unword table) 1)
	finally (return table)))




