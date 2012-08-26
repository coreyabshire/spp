(in-package :util)

; (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro of-these (fn pred &rest them)
  "Use the predicate mapper FN (i.e. every, some, etc...)
   to apply the predict PRED (i.e. evenp, oddp),
   to the inline list THEM."
  `(,fn #',pred (list ,@them)))

(defmacro nullify (&body body)
  "Run BODY for its side effects, and return NIL."
  `(progn ,@body nil))

(defun read-word-file (filename)
  "Collect and return all words, each assumed to be on
its own line, from the file specified by FILENAME."
  (with-open-file (in filename)
    (loop for w = (read-line in nil nil) while w
	  collect w)))

(defun remove-letters (letters word)
  "Remove all the letters in WORD from LETTERS."
  (remove-if (lambda (c) (find c word)) letters))

(defun hash-table-alist (hash-table)
  "Build an alist of HASH-TABLE, mapping keys to values."
  (loop for k being the hash-keys of hash-table
	using (hash-value v)
	collect (cons k v)))

(defun unique (elements)
  "Return an alist of the unique elements of ELEMENTS
associated with their respective counts."
  (loop with h = (make-hash-table :test 'equal)
	for k in elements
	do (incf (gethash k h 0))
	finally (return (hash-table-alist h))))

(defun compose (&rest fns)
  "Create a function that applies every predicate in FNS to its argument."
  (lambda (w) (every (lambda (fn) (funcall fn w)) fns)))

(defun choose-random (seq n)
  "Choose N random elements of SEQ."
  (loop with r = (length seq)
	for i from 0 below n
	collect (elt seq (random r))))

(defun count-repeats (s)
  "Count repeat characters in the string S."
  (loop with u = nil ; unique chars seen
	for c across s
	if (member c u) count c
	else do (pushnew c u)))
	
(defun has-letters-p (word letters)
  "Does WORD contain all the characters in LETTERS?"
  (not (null (some (lambda (c) (find c letters)) word))))

(defmacro def-print-object-with-slots (class &rest slots)
  "Define a new print-object method for objects of the given CLASS
that will print an unreadable object including identity type information,
plus a string that includes the value of each slot in SLOTS."
  (let ((control-string (string-downcase (format nil "~{:~a ~~a~^ ~}" slots))))
    `(defmethod print-object ((object ,class) stream)
       (print-unreadable-object (object stream :type t :identity t)
	 (with-slots (,@slots) object
	   (format stream ,control-string ,@slots))))))

(defmacro with-gensyms ((&rest symbols) &body body)
  "Creates gensyms for each of the specified SYMBOLS."
  `(let ,(mapcar (lambda (s) `(,s (gensym))) symbols)
     ,@body))

