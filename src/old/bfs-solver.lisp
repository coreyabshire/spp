(in-package :palindrome)

(defclass solver-state ()
  ((h :initarg :h)
   (dict :initarg :dict)
   (epals :initarg :epals)
   (opals :initarg :opals)
   (len-epals :initform 0)
   (len-opals :initform 0)
   (letters :initarg :letters)
   (count :initarg :count)))

(defun record-and-check (solver-state p)
  (with-slots (epals opals len-epals len-opals letters) solver-state
    (if (= 0 (length (pal-remainder p)))
	(progn (push p epals)
	       (incf len-epals)
	       (setf letters (remove-letters letters (pal-string p)))
	       (if (= 0 (length letters))
		   (progn ;(format t "have enough from epals~%")
			  nil)
		   (dolist (opal opals nil)
		     (if (= 0 (length (remove-letters letters (pal-string opal))))
			 (progn ;(format t "have enough from epals + opal: ~a~%" opal)
			   ))))
	(progn (push p opals)
	       (incf len-opals)
	       (if (= 0 (length (remove-letters letters (pal-string p))))
		   (progn ;(format t "have enough from epals + new opal: ~a~%" p)
			  nil)))))))
      
(defun print-page-of (solver-state repeats page-num page-size)
  (let ((start (* page-num page-size)))
    (dolist (p (subseq (gethash repeats (slot-value solver-state 'h)) start (+ start page-size)))
      (format t "~a~%" p))))

(defvar *repeats*)
(defvar *page-num*)
(defvar *page-size*)

(defun next-page ()
  (print-page-of *repeats* *page-num* *page-size*)
  (incf *page-num*))
  
(defun first-page (repeats page-num page-size)
  (setq *repeats* repeats
	*page-num* page-num
	*page-size* page-size)
  (next-page))

(defun do-one (n solver-state)
  (with-slots (h dict pals letters count) solver-state
    (let ((p (yank-lowest h)))
      (decf count)
      ;(progn (format t "~a: ~a | ~a~%" n solver-state p) (finish-output))
      (let ((pal (pal-string p)))
	(if (palindromep (list pal))
	    (progn ;(format t "found palindrome: ~a~%" pal)
		   (record-and-check solver-state p))
	    (let ((rem (pal-remainder p)))
	      (if (lhs-heavy-p p)
		  (do-words-ending (w (reverse rem) dict)
;		    (format t "~a, ~a, ~a~%" p rem w)
		    (let ((np (append-rhs p w)))
		      (push np (gethash (repeats np) h))
;		      (format t "~a~%" np)
		      (incf count)))
		  (do-words-starting (w (reverse rem) dict)
;		    (format t "~a, ~a, ~a~%" p rem w)
		    (let ((np (append-lhs p w)))
		      (push np (gethash (repeats np) h))
;		      (format t "~a~%" np)
		      (incf count))))))))))

(defun do-some (n solver-state)
  (loop for i from 1 upto n
	for p = (do-one i solver-state)
	until p)
  (format t "~a~%" (get-counts (slot-value solver-state 'h))))

(defun init-solver-state (dict)
  (let ((h (loop with h = (make-hash-table)
		 for word across (all-words dict)
		 for p = (make-instance 'palindrome :lhs (list word) :rhs ())
		 do (push p (gethash (repeats p) h ()))
		 finally (return h))))
    (make-instance 'solver-state
		   :h h
		   :dict dict
		   :epals ()
		   :opals ()
		   :letters +alphabet+
		   :count (loop for v being the hash-values of h
				sum (length v)))))
  

(defvar *solver-state*)

(defun reset-solver-state ()
  (setf *solver-state* (init-solver-state *dict*)))

(defun make-attempt (solver-state)
  (with-slots (epals opals) solver-state
    (loop with e = (minimize-epals epals)
	  with letters = (remove-all-letters +alphabet+ e)
	  with inner = (find-with-letters opals letters)
	  with result = inner
	  for p in e
	  do (setf result (make-instance 'palindrome
					 :lhs (append (lhs p) (lhs result))
					 :rhs (append (rhs result) (rhs p))))
	  finally (return result))))

; #<PALINDROME :lhs (raj ax om golf live shad skua slug snit snow snug soda sorb sore spam span spar spat spay spaz spec de suq) :rhs (used ceps zaps yaps taps raps naps maps eros bros ados guns wons tins guls auks dahs evil flog moxa jar) :repeats 141>

(defun minimize-epals (epals)
  (loop with letters = +alphabet+
	for p in epals
	for pal = (pal-string p)
	if (some (lambda (c) (find c letters)) pal)
	collect p
	and do (setq letters (remove-letters letters pal))))

(defun find-with-letters (opals letters)
  (find-if (lambda (p) (every (lambda (c) (find c p)) letters))
	   opals
	   :key #'pal-string))

(defun remove-all-letters (letters pals)
  "Create a sequence that has all the characters in PALS, a list
of palindromes, removed from LETTERS."
  (loop with result = (copy-seq letters)
	for p in pals
	do (setq result (remove-letters result (pal-string p)))
	finally (return result)))

(defmethod print-object ((object solver-state) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (h epals opals len-epals len-opals letters count) object
      (format stream ":epals ~a :opals ~a :letters ~a :count ~a"
	      len-epals len-opals letters count))))

(defun yank-lowest (h)
  (let ((k (lowest-key h)))
  (let ((p (pop (gethash k h))))
    (if (null (gethash k h))
	(remhash k h))
    p)))

(defvar *active* (make-hash-table))

(defun lowest-key (h)
  (loop for k being the hash-keys of h
	minimize k))
	
(defun show-keys (h)
  (loop for k being the hash-keys of h
	collect k into keys
	finally (return (sort keys #'<))))

(defun get-counts (h)
  (loop for k being the hash-keys of h using (hash-value v)
	collect (cons k (length v)) into keys
	finally (return (sort keys #'< :key #'car))))

