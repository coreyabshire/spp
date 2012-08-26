;;;; ----------------------------------------------------------------------
;;;; Palindrome Word File Generation Utilities
;;;; ----------------------------------------------------------------------
;;;; In my quest to solve the ITA challenge of finding the shortest
;;;; possible palindrome pangram, I have found that working exlusively
;;;; with the data set they have provided can be problematic and
;;;; prohibitive. Further, analysis on how to generate another data
;;;; file can be enlightening. Thus, I feel it will be beneficial to
;;;; my efforts to spin this concept of test data generation off
;;;; into its own separate module and give it some real effort
;;;; and design.
;;;; ----------------------------------------------------------------------


;;;; ----------------------------------------------------------------------
;;;; Aspects of Test Data Creation
;;;; ----------------------------------------------------------------------
;;;;  - Frequency Distribution of Letters
;;;;  - Size of Words
;;;;  - Number of Seeded Possible Palindrome Pangrams
;;;;  - Length of Seeded Palindrome Pangrams
;;;;  - Average Depth to Solution
;;;;  - Average Solution Length
;;;;  - Branching Factor
;;;; ----------------------------------------------------------------------


;;;; ----------------------------------------------------------------------
;;;; Test Cases
;;;; ----------------------------------------------------------------------
;;;;  - The full ITA provided dataset.
;;;;  - A sample of the full ITA dataset.
;;;;  - A dictionary from another source.
;;;;  - Each of the 26 letters as a separate, single character word.
;;;;  - Small words (various letter combinations) with 1 seed.
;;;;  - A palindrome pangram as a single word.
;;;;  - An empty word set.
;;;;  - A single letter.
;;;;  - A random palindrome pangram as a broken up into chunks.
;;;; ----------------------------------------------------------------------


(in-package :spp)

'(testcase
  :name '(26 one letter words)
  :purpose "Require a large depth with a known good data set."
  :wordlist-form (loop for c across *letters* collect (string c)))

(defun random-letter (letters)
  (char letters (random (length letters))))

(defun random-word (n letters)
  (loop for i from 1 upto n
	collect (random-letter letters) into w
	finally (return (map 'string #'identity w))))

(defun random-words (n m letters)
  (loop for i from 1 upto n
	collect (random-word m letters) into w
	finally (return (map 'vector #'identity w))))

(defun shuffle-word (word)
  (let ((n (length word))
	(s (copy-seq word)))
    (dotimes (i n s)
      (rotatef (char s (random n))
	       (char s (random n))))))

(defun pick-random (words n)
  "Pick N random words from WORDS."
  (loop with max = (length words)
	for i from 1 upto n
	for j = (random max)
	collect (aref words j) into list
	finally (return (map 'vector #'identity list))))

(defun chunk-word (word &rest sizes)
  (let ((s (reduce #'+ sizes))
	(n (length word)))
    (if (< s n)
	(setf sizes (append sizes (list (- n s))))))
  (loop for size in sizes
	for start = 0 then end
	for end = size then (+ end size)
	collect (subseq word start end) into chunks
	finally (return (map 'vector #'identity chunks))))

(defun random-spp-chunks ()
  (let ((base (shuffle-word *letters*)))
    (let ((spp (concatenate 'string base (reverse base))))
      (chunk-word spp 7 8 2 3 9 4 7 4 8))))

(defun random-easy-chunks ()
  (let ((base *letters*))
    (let ((spp (concatenate 'string base (reverse base))))
      (chunk-word spp 7 8 2 3 9 4 7 4 8))))

(defun write-random-data-file (n m letters basename)
  (with-open-file (out (data-file basename)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (loop for word across (random-words n m letters) do
	  (format out "~a~%" word))))

(defmacro with-simple-output ((out path) &body body)
  `(with-open-file (,out ,path
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
     ,@body))

(defun write-random-selection (basename words n)
  (with-simple-output (out (data-file basename))
    (loop for i from 1 upto n do
	  (format out "~a~%" (aref words (random (length words)))))))

(defun write-data-file (basename words)
  (with-open-file (out (data-file basename)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (loop for word across words do
	  (format out "~a~%" word))))

(defclass spptest ()
  ((name
    :initarg :name)
   (documentation
    :initarg :documentation)
   (wordsform
    :initarg :wordsform)))

(defvar *spptests*
  (make-hash-table :test #'equal))

(defmacro defspptest (name (&rest args) &key documentation wordsform)
  `(setf (gethash ',name *spptests*)
	 (make-instance 'spptest
			:name ',name
			:documentation ,documentation
			:wordsform ,wordsform)))

(defmethod test-words (name)
  (funcall (slot-value (gethash name *spptests*) 'wordsform)))

(defspptest single-letters ()
  :documentation
  "This test has a known and obvious SPP, but forces
  the algorithm to go to a depth of at least 51 to find
  it. This should be almost instantaneous to run if things
  are working properly, and thus it should be easy to tell
  if something is broke."
  :wordsform
  (lambda () (map 'list #'string *letters*)))

(defstruct spp-run-log-entry
  run-id
  start-time
  end-time
  heuristic-used
  successor-used
  search-algorithm-used
  test-data-used
  state-count
  interrupted-p
  expansion-log
  max-queue-size
  max-depth
  nodes-created-per-expansion
  solution-depth
  branching-factor
  effective-branching-factor
  expansion-count
  node-creation-count
  solution)

(defclass lab-report ()
  (hypothesis
   timestamp
   configuration
   procedure
   test-data
   notes
   conclusion))
   
'(:hypothesis
  "Branching factor can be decreased by removing
  filtering out words that have no characters that
  we need, while not removing potentially valid
  and more optimal paths.")

(defparameter *bug-palindrome-ends-in-middle*
  (list "abc"
	"defghijk"
	"lmnop"
	"qrstu"
	"vwxy"
	;"abcdefz"
	;"fedcb"
	;"ayx"
	"zyxwvutsrqp"
	"onmlkji"
	"hgfedcba"))

(defun alternate-palindrome-test (candidate)
  (let* ((forward (remove #\Space candidate))
	 (reverse (reverse forward)))
    (string= forward reverse)))

(defun alternate-pangram-test (candidate)
  (every (lambda (c) (find c candidate)) *letters*))

(defun alternate-goal-test (candidate)
  (and (alternate-pangram-test candidate)
       (alternate-palindrome-test candidate)))

(defun join-space (words)
  (format nil "~{~a~^ ~}" words))

(defparameter *test-data-remove-unhelpful-words*
  (list "abc"
	"defghijk"
	"lmnop"
	"qrstu"
	"vwxy"
	"abcdefz"
	"fedcba"
	"yxwvutsrqp"
	"onmlkji"
	"hgfedcba"))
  