(in-package :bsearch)

(defun bs-position-if (word sequence &key (test #'string=) (compare #'string<)
		       (start 0) (end (- (length sequence) 1)) (from-end nil))
  "Find the first (unless FROM-END, in which case find the last)
   position of WORD in SEQUENCE matching TEST between START and END,
   where SEQUENCE is sorted by COMPARE."
  (if (<= start end)
      (let ((mid (+ start (floor (- end start) 2))))
	(let ((s (aref sequence mid)))
	  (if (funcall test word s)
	      ;; It passed the test, but is it really the first?
	      ;; (or last, depending on from-end...)
	      (or (if (not from-end)
		      (bs-position-if word sequence :test test :compare compare
				      :start start :end (- mid 1) :from-end from-end)
		      (bs-position-if word sequence :test test :compare compare
				      :start (+ mid 1) :end end :from-end from-end))
		  mid)
	      ;; It didn't even pass the test, so keep searching based on
	      ;; however it happens to be sorted (according to compare).
	      (if (funcall compare word s)
		  (bs-position-if word sequence :test test :compare compare
				  :start start :end (- mid 1) :from-end from-end)
		  (bs-position-if word sequence :test test :compare compare
				  :start (+ mid 1) :end end :from-end from-end)))))))

