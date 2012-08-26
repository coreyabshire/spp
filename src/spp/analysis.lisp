;;;; -*- Mode: Lisp; -*- 
;;;;
;;;; Shortest Palindromic Pangram Problem Domain (ITA) - dca
;;;;

(in-package :spp)

(defun expected-num-nodes (b d)
  (loop
     for i from 0 upto d
     sum (expt b i)))

(defun effective-branching-factor (d n &optional (err 0.0001))
  (let ((b 1) max min (errn (* n err)) (targetn (+ n 1)))
    (loop
       for g = (expected-num-nodes b d)
       until (< (abs (- g targetn)) errn)
       if (> g targetn) do (setq max b)
       if (< g targetn) do (setq min b)
       do (setq b (cond ((null max) (* b 2.0))
			((null min) (/ b 2.0))
			(t (/ (+ max min) 2.0))))
       do (format t "~a ~a~%" b g)
       finally (return b))))
       

(defparameter *all-algorithms*
  '(a*-search tree-ida*-search))

(defun solution-length (solution)
  (length (remove #\Space solution)))

(defun compare (algorithms basenames)
  "Run each of the find-spp calls, collect stats, and print a report."
  (format t "~20a ~5a ~5a ~8a ~8a ~8a~%"
	  "Description" "Run" "Real" "Words" "Length" "Expanded")
  (format t "~20a ~5a ~5a ~8a ~8a ~8a~%"
	  "-----------" "---" "----" "-----" "------" "--------")
  (loop for basename in basenames
     do (loop for algorithm in algorithms
	   for start-run-time = (get-internal-run-time)
	   for start-real-time = (get-internal-real-time)
	   for solution = (find-spp :basename basename :algorithm algorithm)
	   for end-run-time = (get-internal-run-time)
	   for end-real-time = (get-internal-real-time)
	   for elapsed-run-time = (- end-run-time start-run-time)
	   for elapsed-real-time = (- end-real-time start-real-time)
	   for num-words = (length (words-forward *problem*))
	   for length = (solution-length solution)
	   for num-expanded = (num-expanded *problem*)
	   do (format t "~20a ~5d ~5d ~8d ~8d ~8d~%"
		      algorithm
		      elapsed-run-time elapsed-real-time
		      num-words length num-expanded))))



;;;; ---------------------------------------------------------------
;;;; Analysis of the words.
;;;; ---------------------------------------------------------------

