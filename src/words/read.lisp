;; -*- Mode: Lisp; -*- 

;;;; Simple utility to read in a list of words.

(in-package :words)

(defun read-words (filespec)
  "Read in all the words from a file containing a linefeed delimited word list."
  (with-open-file (stream filespec)
    (loop for w = (read-line stream nil nil) while w
	  collect w)))

