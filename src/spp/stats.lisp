(in-package :spp)

(defun collect-word-stats (out wordlist &key (letters *letters*))
  (let ((letter-counts (make-hash-table :size (length letters)))
	(letter-words-having-counts (make-hash-table :size (length letters)))
	(letter-min-word-length (make-hash-table :size (length letters)))
	(letter-max-word-length (make-hash-table :size (length letters)))
	(letter-sum-word-length (make-hash-table :size (length letters)))
	(length-count (make-hash-table))
	(dup-count-frequency (make-hash-table))
	(min-dup-count 255)
	(max-dup-count 0)
	(sum-dup-count 0)
	(dup-count-count 0)
	(min-word-length 255)
	(max-word-length 0)
	(all-seen ())
	)
    (loop for letter across letters do
	  (setf (gethash letter letter-counts) 0)
	  (setf (gethash letter letter-words-having-counts) 0)
	  (setf (gethash letter letter-min-word-length) 255)
	  (setf (gethash letter letter-max-word-length) 0)
	  (setf (gethash letter letter-sum-word-length) 0))
    (let ((control-string "    ~20a ~10d~%"))
      (format out "overall stats:~%")
      (loop for word in wordlist
	    for word-length = (length word)
	    count word into word-count
	    sum word-length into sum-word-length
	    if (> word-length max-word-length) do (setf max-word-length word-length)
	    if (< word-length min-word-length) do (setf min-word-length word-length)
	    do (loop with seen = () with dups = 0 for c across word do
		     (incf (gethash c letter-counts))
		     (if (member c seen)
			 (incf dups)
			 (progn (pushnew c seen)
				(pushnew c all-seen)))
		     finally (progn (loop for c in seen do
					  (incf (gethash c letter-words-having-counts))
					  (if (< word-length (gethash c letter-min-word-length))
					      (setf (gethash c letter-min-word-length) word-length))
					  (if (> word-length (gethash c letter-max-word-length))
					      (setf (gethash c letter-max-word-length) word-length))
					  (incf (gethash c letter-sum-word-length) word-length))
				    (incf (gethash dups dup-count-frequency 0))
				    (if (< dups min-dup-count) (setf min-dup-count dups))
				    (if (> dups max-dup-count) (setf max-dup-count dups))
				    (incf sum-dup-count dups)
				    (if (> dups 0) (incf dup-count-count))))
	    do (incf (gethash word-length length-count 0))
	    finally (progn (format out control-string "word-count" word-count)
			   (format out control-string "min-word-length" min-word-length)
			   (format out control-string "max-word-length" max-word-length)
			   (format out control-string "sum-word-length" sum-word-length)
			   (format out control-string "avg-word-length" (floor sum-word-length word-count))
			   (format out control-string "all-seen-length" (length all-seen))
			   (format out control-string "dup-count-count" dup-count-count)
			   (format out control-string "min-dup-count" min-dup-count)
			   (format out control-string "max-dup-count" max-dup-count)
			   (format out control-string "sum-dup-count" sum-dup-count)
			   )))
    (format out "all seen:~%")
    (format out "    ~20a ~{~a~}~%" "all-seen" (setf all-seen (sort all-seen #'char<)))
    (format out "    ~20a ~{~a~}~%" "all-seen-missing" (loop for c across letters if (not (member c all-seen)) collect c))
    (format out "letter stats:~%")
    (format out "    ~6a ~6a ~6a ~6a ~6a ~8a~%" "letter" "count" "words" "minlen" "maxlen" "sumlen")
    (let ((control-string "    ~6a ~6d ~6d ~6d ~6d ~8d~%"))
      (loop for letter across letters do
	    (format out control-string
		    letter
		    (gethash letter letter-counts)
		    (gethash letter letter-words-having-counts)
		    (gethash letter letter-min-word-length)
		    (gethash letter letter-max-word-length)
		    (gethash letter letter-sum-word-length)
		    )))
    (format out "length stats:~%")
    (format out "    ~6a ~6a~%" "length" "count")
    (let ((control-string "    ~6a ~6d~%"))
      (loop for length from min-word-length upto max-word-length do
	    (format out control-string
		    length
		    (gethash length length-count 0))))
    (format out "duplicate letters frequency:~%")
    (format out "    ~6a ~6a~%" "dups" "count")
    (let ((control-string "    ~6a ~6d~%"))
      (loop for dups from min-dup-count upto max-dup-count do
	    (format out control-string
		    dups
		    (gethash dups dup-count-frequency 0))))
    ))
  
	
