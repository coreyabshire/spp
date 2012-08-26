(defpackage :dictionary
  (:use :common-lisp :bsearch :utils)
  (:export #:dictionary
	   #:make-dictionary
	   #:all-words
	   #:load-dictionary
	   #:do-words-starting
	   #:do-words-ending
	   #:words-starting
	   #:words-ending
	   #:has-word-starting
	   #:has-word-ending
	   #:pick-one))

(in-package :dictionary)

;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defclass dictionary ()
  ((words :initarg :words)
   (rwords :initarg :rwords)
   (length :initarg :length)))

(defclass word-range ()
  ((start :initarg :start :initform nil)
   (end :initarg :end :initform nil)))

(defmethod print-object ((object word-range) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (start end) object
      (format stream ":start ~a :end ~a" start end))))

;(defclass partial-word ()
;  (words-starting-with)
;  (words-ending-with))

(defun build-index (words)
  (loop with hash-table = (make-hash-table :test #'equal)
	for word across words
	for i upfrom 0
	do (loop for end from 0 upto (length word)
		 for key = (subseq word 0 end)
		 for range = (or (gethash key hash-table)
				 (setf (gethash key hash-table)
				       (make-instance 'word-range :start i)))
		 do (setf (slot-value range 'end) i))
	finally (return hash-table)))

(defun make-dictionary (word-list)
    (make-instance 'dictionary
		   :words (sort (map 'vector #'identity word-list) #'string<)
		   :rwords (sort (map 'vector #'reverse word-list) #'string<)
		   :length (length word-list)))

(defun load-dictionary (filename)
  (make-dictionary (read-word-file filename)))

(defun position-starting (dict word &optional (start 0))
  (bs-position-if word (slot-value dict 'words) start (1- (slot-value dict 'length))))

(defun position-ending (dict word &optional (start 0))
  (bs-position-if (reverse word) (slot-value dict 'rwords) start (1- (slot-value dict 'length))))

(defmacro do-words-starting ((w word dict) &body body)
  `(with-slots (words rwords length) ,dict
     (loop for i = (bs-position-if ,word words 0 (1- length))
	   then (bs-position-if ,word words (1+ i) (1- length)) while i
	   for ,w = (aref words i)
	   do (progn ,@body))))

(defmacro do-words-ending ((w word dict) &body body)
  `(with-slots (words rwords length) ,dict
     (loop for i = (bs-position-if (reverse ,word) rwords 0 (1- length))
	   then (bs-position-if (reverse ,word) rwords (1+ i) (1- length)) while i
	   for ,w = (reverse (aref rwords i))
	   do (progn ,@body))))

(defun has-word-starting (word dict)
  (with-slots (words length) dict
    (not (null (bs-position-if word words 0 length)))))

(defun has-word-ending (word dict)
  (with-slots (rwords length) dict
    (not (null (bs-position-if (reverse word) rwords 0 length)))))

(defun words-ending (word dict)
  (let ((matches nil))
    (do-words-ending (match word dict)
      (push match matches))
    (nreverse matches)))

(defun words-starting (word dict)
  (let ((matches nil))
    (do-words-starting (match word dict)
      (push match matches))
    (nreverse matches)))

(defun pick-one (dict)
  (with-slots (words length) dict
    (aref words (random length))))

(defun all-words (dict)
  (slot-value dict 'words))

(defun all-rwords (dict)
  (slot-value dict 'rwords))

(defmethod print-object ((object dictionary) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (length) object
      (format stream "~d words" length))))

