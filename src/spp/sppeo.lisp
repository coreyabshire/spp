;;;--------------------------------------------------------------------------------
;;; find shortest even odd combo
;;;--------------------------------------------------------------------------------

(defstruct (evens-odd-problem (:include problem) (:conc-name "EOP-")) evens)

(defun combine-even-odd (even odd)
  ;; odd  :lhs aplan aman         :rhs acanal panama
  ;; even :lhs me live            :rhs em evil
  ;; comb :lhs aplan aman me live :rhs acanal panama em evil
  (let ((need (remove-if (lambda (c) (member c (seen even))) (need odd))))
    (make-state :lhs (append (lhs odd) (lhs even))
		:rhs (append (rhs odd) (rhs even))
		:ln (+ (ln odd) (ln even))
		:rn (+ (rn odd) (rn even))
		:seen (append (seen odd) (remove-if (lambda (c) (member c (seen odd))) (seen even)))
		:need need
		:cache-h-cost (1- (* 2 (length need))))))

(defmethod successors ((problem evens-odd-problem) state)
  (loop for even in (eop-evens problem)
	if (member (car (need state)) (seen even))
	collect (cons even (combine-even-odd even state))))

(defmethod goal-test ((problem evens-odd-problem) state)
  (and (pangramp state)
       (palindromep state)))

(defmethod h-cost ((problem evens-odd-problem) state)
  (1- (* 2 (length (need state)))))

(defmethod edge-cost ((problem pangram-problem) node action state)
  (+ (ln action) (rn action)))
	
(defun solve-evens-odd (evens odd)
  (if (and evens odd)
      (let* ((problem (make-evens-odd-problem :initial-state odd :evens evens))
	     (solution-node (solve problem 'tree-sma nil)))
	(cond ((null solution-node) nil)
	      (t (format t "solution found in evens odd: ~a~%" (node-state solution-node))
		 (finish-output))))))



