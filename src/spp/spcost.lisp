;;;; 
;;;; hcost: Pangram cost table generator.
;;;;
;;;; Program to find the cost in terms of the sum of the lengths
;;;; of a sequence of words, to find a sequence of words from an
;;;; overall dictionary, that has all the letters in a  given range
;;;; of letters, for all permutations of the letters of the
;;;; alphabet.
;;;;
;;;; Because this is a CPU intensive computation that can run for
;;;; many hours, this program supports writing the cost table out
;;;; to disk in case it is necessary to interrupt it, and so that
;;;; once its finished the computation does not need to be
;;;; repeated.
;;;;



;;; ----------------------------------------------------------------------
;;;  package definition
;;; ----------------------------------------------------------------------

(in-package :spp)

;;; ----------------------------------------------------------------------
;;;  
;;; ----------------------------------------------------------------------

(defparameter *cost-count* (expt 2 *nletters*)
  "The maximum number of letter combinations.")

(defun init-costs ()
  "Construct and return a new costs table."
  (let ((costs (make-array *cost-count*
			   :initial-element 255
			   :element-type '(unsigned-byte 8))))
    (setf (aref costs 0) 0)
    costs))

(declaim ((simple-array (unsigned-byte 8)) *costs*))

(defvar *costs* (init-costs)
  "A table of the shortest pangram for each word sig possible
  for the alphabet.")


(defvar *sigs* ())

(defvar *sigs2* ())

;;; ----------------------------------------------------------------------
;;;  main cost function
;;; ----------------------------------------------------------------------

(defun process-costs-all ()
  (loop for n from 2 upto 24
	for c = (1- (expt 2 (1- n)))
	for k = (time (process-costs-n n))
	do (format t "~a: ~a x ~a = ~a ~%~%" n k c (* k c))
	do (finish-output)))

(defun reset-costs-words (costs words)
  (loop for word in words
	for sig = (word-sig word)
	for pcost = (length word)
	for ccost = (aref costs sig)
	do (progn (if (< pcost ccost)
		      (setf (aref costs sig) pcost))
		  (loop for c across word
			for csig = (word-sig (string c))
			if (< pcost (aref costs csig))
			do (setf (aref costs csig) pcost)))))

(defun process-costs-1 ()
  (loop with n = 1
	for c1 across *letters*
	do (loop for c2 across *letters*
		 if (char< c1 c2)
		 do (let* ((sig (word-sig (format nil "~a~a" c1 c2)))
			   (pcost (+ (aref *costs* (word-sig (string c1)))
				     (aref *costs* (word-sig (string c2)))))
			   (ccost (aref *costs*  sig)))
		      (if (or (= ccost 0) (< pcost ccost))
			  (progn (setf (aref *costs* sig) pcost)
				 (format t "updated cost :n ~a :old ~a :new ~a :word ~a~a :sig ~a~%"
					 (incf n) ccost pcost c1 c2 sig)))))))
#|
(defun process-costs-n (n)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
	   (fixnum n))
  (loop for sig from 1 to (expt 2 26)
	if (= (logcount sig) n)
	count (loop ;with bits = (split-sig sig)
		    for i from 1 to (1- (the fixnum (expt 2 (1- n))))
		    for a of-type (unsigned-byte 32) = (logdist i sig)
		    for b of-type (unsigned-byte 32) = (logxor a sig)
		    for pcost of-type (unsigned-byte 8) = (+ (aref *costs* a) (aref *costs* b))
		    for ccost of-type (unsigned-byte 8) = (aref *costs* sig)
		    while (> ccost n)
		    if (< pcost ccost)
		    do (setf (aref *costs* sig) pcost)
		    finally (return 1))))
|#
(defun process-costs-n (n)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
	   (fixnum n))
  (loop for sig from 1 to (expt 2 26)
	if (= (logcount sig) n)
	count (loop for i from 1 to (1- (the fixnum (expt 2 (1- n))))
		    for a of-type (unsigned-byte 32) = (logdist i sig)
		    for b of-type (unsigned-byte 32) = (logxor a sig)
		    for pcost of-type (unsigned-byte 8) = (+ (aref *costs* a) (aref *costs* b))
		    for ccost of-type (unsigned-byte 8) = (aref *costs* sig)
		    while (or (= ccost 0) (> ccost n))
		    if (or (= ccost 0) (< pcost ccost))
		    do (setf (aref *costs* sig) pcost)
		    finally (return 1))))

(defun print-single-letter-costs ()
  (loop for c across *letters*
	for s = (word-sig (string c))
	do (format t "~a: ~a~%" c (aref *costs* s))))

(defun reset-costs ()
  (loop for i from 0 to *cost-count*
	for x = (aref *costs* i)
	if (> x 0)
	do (loop for j in *sigs*
		 for y = (aref *costs* j)
		 for k = (logior i j)
		 for z = (+ x y)
		 for c = (aref *costs* k)
		 if (or (= c 0) (< z c))
		 do (setf (aref *costs* k) z))))
		 
(defstruct sig-node words cost)

(defvar *sig-table* (make-hash-table))

(defun init-sigs (table)
  (loop for word across *words*	for sig = (word-sig word) for cost = (length word) do
	(multiple-value-bind (node hit) (gethash sig table)
	  (if (or (not hit) (< cost (sig-node-cost node)))
	      (setf (gethash sig table) (make-sig-node :words (list word) :cost cost))))))
	
(defun extend-sigs (table)
  (loop for word across *words*
	for sig = (word-sig word)
	for cost = (length word) do
	(loop for sig2 being the hash-keys of table using (hash-value node)
	      for rsig = (logior sig sig2)
	      for rcost = (+ cost (sig-node-cost node))
	      for rwords = (sig-node-words node) do
	      (multiple-value-bind (node hit) (gethash rsig table)
		(if (or (not hit) (< rcost (sig-node-cost node)))
		    (setf (gethash rsig table) (make-sig-node :words (cons word rwords) :cost cost)))))))
	      

(defparameter *costs-filename* "/home/corey/SPP/data/costs.dat")

(defun save-costs ()
  (with-open-file (out *costs-filename* :element-type '(unsigned-byte 8) :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (write-sequence *costs* out))
  'done)

(defun load-costs ()
  (with-open-file (in *costs-filename* :element-type '(unsigned-byte 8))
    (read-sequence *costs* in))
  'done)

(defparameter *word-sig-table* (make-hash-table :test #'equal :size 174000))

