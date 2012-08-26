;;;; ----------------------------------------------------------------------
;;;; Binary word search macros.
;;;; ----------------------------------------------------------------------
;;;; This file contains the implementation of a handful of word search
;;;; functions. The functions are based on having a sorted set and finding
;;;; matches using a binary search approach.
;;;; ----------------------------------------------------------------------

(in-package :words)

;;;; ----------------------------------------------------------------------
;;;; DO-WORDS-STARTING and DO-WORDS-ENDING macros
;;;; ----------------------------------------------------------------------
;;;; This is the main implementation of both DO-WORDS-STARTING and
;;;; DO-WORDS-ENDING. I found in implementing the first version of
;;;; both of those macros that they looked the same except for the
;;;; following differences:
;;;;   - prefixes vs suffixes for exact match (smaller fn)
;;;;   - starts with or ends with (test fn)
;;;;   - regular sort vs. reverse word sort (compare fn)
;;;; To help eliminate bugs and maximize code reuse I decided to
;;;; extract that code out into this macro, and have those other
;;;; two macros call this one instead.
;;;; ----------------------------------------------------------------------

(defmacro subseq-iterator (sequence i start end)
  (with-gensyms (n seq r)
    `(let* ((,seq ,sequence)
	    (,n (length ,seq))
	    (,i 1))
       (lambda ()
	 (when (< ,i ,n)
	   (let ((,r (subseq ,sequence ,start ,end)))
	     (incf ,i)
	     ,r))))))

(defun iterator-match-iterator (iter seq)
  (lambda ()
    (loop for w = (funcall iter) while w
	  for p = (bs-position-if w seq) until p
	  finally (return p))))

(defun subseq-match-iterator (s seq)
  (let ((f (prefix-iterator s)))
    (lambda ()
      (loop for w = (funcall f) while w
	    for p = (bs-position-if w seq) until p
	    finally (return p)))))

(defun range-iterator (s seq test compare)
  (let ((from (bs-position-if s seq :test test :compare compare))
	(upto (bs-position-if s seq :test test :compare compare :from-end t)))
    (let ((i from))
      (lambda ()
	(if (and from (<= i upto))
	    (let ((j i))
	      (incf i)
	      j))))))

(defun merge-iterators (&rest iterators)
  (lambda ()
    (labels ((next (iterators)
	       (if iterators
		   (or (funcall (car iterators))
		       (next (cdr iterators))))))
      (next iterators))))

(defun prefix-iterator (s)
  "Returns an iterator over the prefixes of S."
  (subseq-iterator s i 0 i))

(defun suffix-iterator (s)
  "Returns an iterator over the suffixes of S."
  (subseq-iterator s i i nil))

(defun position-iterator (s seq op)
  "Returns a procedure that when called returns the next position
  in sequence matching target, or NIL if there are no more."
  (multiple-value-bind (smaller test compare)
      (cond ((string= (symbol-name op) "STARTS")
	     (values (prefix-iterator s) #'word-starts-with #'string<))
	    ((string= (symbol-name op) "ENDS")
	     (values (suffix-iterator s) #'word-ends-with #'reverse-string<)))
    (merge-iterators (iterator-match-iterator smaller seq)
		     (range-iterator s seq test compare))))

(defmacro enforce-extra-keywords (&rest keywords)
  (loop for k in keywords
	collect `(unless (eq ',k ',k) (error "syntax error: ~a unexpected" ,k)) into body
	finally (return `(progn ,@body))))
	   
(defun keyword-symbol-one-of (symbol &rest strings)
  (member (symbol-name symbol) (mapcar #'string-upcase strings) :test #'string=))

(defmacro word-loop (where word op with arg in words &body keywords-and-forms)
  "Expands into a loop that sets up a position iterator for ARG using OP,
  and then binds WORD to the item in WORDS at each matching position.
  Then rest of the loop body is then included, which is expected to be
  in the syntax of the LOOP macro."
  (unless (and (keyword-symbol-one-of where "WHERE")
	       (keyword-symbol-one-of in "IN")
	       (keyword-symbol-one-of op "STARTS" "ENDS")
	       (keyword-symbol-one-of with "WITH"))
    (error "syntax error in word-loop"))
  (with-gensyms (f p)
    `(loop with ,f = (position-iterator ,arg ,words ',op)
	   for ,p = (funcall ,f) while ,p
	   for ,word = (aref ,words ,p)
	   ,@keywords-and-forms)))

(defmacro do-words-matching ((w target sequence result smaller test compare) &body body)
  "This macro finds either all words W starting or all words ending in TARGET in SEQUENCE.
  SEQUENCE is expected to have been sorted by COMPARE.
  Whether it does starting or ending depends on the function used to determine
  SMALLER portions of TARGET to match, the TEST used to determine whether its
  a match or not, and the function that was used to COMPARE elements in order
  to sort SEQUENCE. For each match, it binds W to the match found and executes BODY."
  (with-gensyms (s seq p start end last)
    ;; First, evaluate TARGET and SEQUENCE once and only once
    ;; and use that for the search rather than the raw forms.
    `(let ((,s ,target) (,seq ,sequence) (,last nil))
       ;; If there is a smaller section of TARGET that has an
       ;; exact match in SEQUENCE, we consider it a match.
       ;; For instance, "do" starts with "done", but "doe" does not.
       (loop for ,w in (funcall ,smaller ,s)
	     ;; huge bug was here (missed compare)
	     for ,p = (bs-position-if ,w ,seq :test #'string= :compare ,compare)
	     if ,p do (setf ,last (progn ,@body)))
       ;; Next, every word from the first match of TARGET in
       ;; SEQUENCE to the last match of TARGET in SEQUENCE,
       ;; will also be a match, since SEQUENCE is sorted. Thus,
       ;; we also consider each element in this range a match
       ;; and so execute BODY for each.
       (let ((,start (bs-position-if ,s ,seq :test ,test :compare ,compare))
	     (,end (bs-position-if ,s ,seq :test ,test :compare ,compare :from-end t)))
	 ;; Neither START nor END may be NULL. There must have been a match
	 ;; on both ends, even if its the same element (single match).
	 (if (and ,start ,end)
	     ;; Execute BODY for each W in the range.
	     (loop for ,p from ,start upto ,end
		   for ,w = (aref ,seq ,p)
		   do (setf ,last (progn ,@body)))))
       ;; Finally, execute the result form and return the result.
       ;; Or, if no result form was specified, return the last
       ;; value of BODY.
       ,(or result last))))

;;;; ----------------------------------------------------------------------
;;;; Modules to plug in to the main macro to provide the specific
;;;; routine requested (starting vs ending).
;;;; ----------------------------------------------------------------------

;;;; Options for SMALLER argument to DO-WORDS-MATCHING.

(defun smaller-starting (word)
  "Returns all the prefixes possible of word."
  (loop for i from 1 upto (- (length word) 1)
	collect (subseq word 0 i)))

(defun smaller-ending (word)
  "Returns all the suffixes possible of word."
  (loop for i from 1 upto (- (length word) 1)
	collect (subseq word i)))

;;;; Options for TEST argument to DO-WORDS-MATCHING.

(defun word-starts-with (s1 s2)
  "Does S2 start with S1?"
  (let ((n1 (length s1))
	(n2 (length s2)))
    (if (>= n2 n1)
	(string= (subseq s2 0 n1) s1))))
	;(string= (subseq s1 0 n2) s2))))

(defun word-ends-with (s1 s2)
  "Does S2 end with S1?"
  (let ((n1 (length s1))
	(n2 (length s2)))
    (if (>= n2 n1)
	(string= (subseq s2 (- n2 n1)) s1))))
	;(string= (subseq s1 (- n1 n2)) s2))))

;;;; Options for the COMPARE argument to DO-WORDS-MATCHING.
;;;; We only to define one option ourselves, since the opposite,
;;;; string< is already provided by Common Lisp.

(defun reverse-string< (string1 string2)
  (string< (reverse string1) (reverse string2)))


;;;; ----------------------------------------------------------------------
;;;; WORDS structure
;;;; ----------------------------------------------------------------------
;;;; A simple structure to allow the reversed words and the sorted word
;;;; vector be contained in the same object so that clients don't have
;;;; to maintain both lists.
;;;; ----------------------------------------------------------------------

(defstruct words
  forward
  reverse)

(defun make-words-from-list (wordlist)
  (let ((words (map 'vector #'identity wordlist)))
    (make-words :forward (sort (copy-seq words) #'string<)
		:reverse (sort (copy-seq words) #'string< :key #'reverse))))

(defun make-words-from-file (filespec)
  (make-words-from-list (read-words filespec)))

;;;; ----------------------------------------------------------------------
;;;; DO-WORDS-XXX Macros
;;;; ----------------------------------------------------------------------
;;;; These are the main macros exported from the packages.
;;;; Users can iterate a subset of words contained in the set
;;;; based on some prefix or suffix.
;;;; ----------------------------------------------------------------------

(defmacro do-words-starting ((w target words &optional result) &body body)
  "Evaluates BODY with W bound to each element of SEQUENCE that
  starts with TARGET. After each such execution of BODY, it
  evaluates and returns RESULT."
  `(do-words-matching (,w ,target (words-forward ,words) ,result
			  #'smaller-starting
			  #'word-starts-with
			  #'string<)
     ,@body))

(defmacro do-words-ending ((w target words &optional result) &body body)
  `(do-words-matching (,w ,target (words-reverse ,words) ,result
			  #'smaller-ending
			  #'word-ends-with
			  #'reverse-string<)
     ,@body))

;;;; ----------------------------------------------------------------------
;;;; HAS-WORD-XXX Tests
;;;; ----------------------------------------------------------------------
;;;; These functions allow a quick check of SEQUENCE to find out if there
;;;; are any words starting or ending without checking all the words.
;;;; ----------------------------------------------------------------------

(defun has-word-ending (target words)
  (let ((sequence (words-reverse words)))
    (or (some (lambda (x) (bs-position-if x sequence :test #'string= :compare #'reverse-string<))
	      (smaller-ending target))
	(bs-position-if target sequence :test #'word-ends-with :compare #'reverse-string<))))

(defun has-word-starting (target words)
  (let ((sequence (words-forward words)))
    (or (some (lambda (x) (bs-position-if x sequence :test #'string= :compare #'string<))
	      (smaller-starting target))
	(bs-position-if target sequence :test #'word-starts-with :compare #'string<))))

;;;; ----------------------------------------------------------------------
;;;; List returning wrapper functions for both macros.
;;;; ----------------------------------------------------------------------
;;;; In some use cases it may be more convenient to simply return a
;;;; list of the matches instead of evaluating an expression over
;;;; the matches. These two functions do just that.
;;;; ----------------------------------------------------------------------

(defun words-starting (target words)
  (let ((result ()))
    (do-words-starting (w target words (reverse result))
      (push w result))))

(defun words-ending (target words)
  (let ((result ()))
    (do-words-ending (w target words (reverse result))
      (push w result))))

