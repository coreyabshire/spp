(in-package :wordset)

(defclass wordset ()
  ((wf :initarg :wf)
   (wr :initarg :wr)))

(defun make-wordset (wordlist)
  (let* ((words (map 'vector #'identity wordlist))
	 (wf (sort (copy-seq words) #'string<))
	 (wr (sort (copy-seq words) #'string< :key #'reverse)))
    (make-instance 'wordset :wf wf :wr wr)))

(defun load-wordset-from-file (word-file)
  (make-wordset (read-words word-file)))
