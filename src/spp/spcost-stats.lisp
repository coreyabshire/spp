(defvar *log-counts*)

(defun init-log-counts ()
  (setf *log-counts* (make-hash-table :size 26))
  (loop for i from 0 upto (expt 2 *nletters*)
	do (incf (gethash (logcount i) *log-counts* 0))))
		 
(defun choice-count (bit-count)
  (loop for i from 0 upto (ceiling bit-count 2)
	sum (ncr bit-count i)))

(defun choice-count (k)
  (- (/ (expt 2 k) 2) 1))

(defun cost-table-complexity ()
  (loop for k being the hash-keys of *log-counts* using (hash-value v)
	for c = (choice-count k)
	sum (* v c)
	do (format t "~a ~a ~a ~a~%" k v c (* v c))))

(defun cost-table-complexity-2 ()
  (loop for k from 1 to 26
	for v = (ncr 26 k)
	for c = (- (/ (expt 2 k) 2) 1)
	sum (* v c)
	do (format t "~3d ~9d ~9d ~15d~%" k v c (* v c))))
	
(defun cost-level-1 ()
  (loop for c in (mapcar 
	for sig = (word-sig (string c))


