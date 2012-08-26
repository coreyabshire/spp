(defun other-side (side)
  (if (eq side 'lhs) 'rhs 'lhs))

(defun new-seen (old-seen word)
  (let ((seen old-seen))
    (loop for c across word
	  unless (member c seen)
	  do (setq seen (cons c seen))
	  finally (return seen))))

(defun words-length (words)
  (loop for w in words
	sum (length w)))

; (defun diff (state)
;   (with-accessors ((lhs lhs) (rhs rhs) (ln ln) (rn rn)) state
;     (if (>= ln rn)
; 	(let ((w (car lhs))) (subseq w (- (length w) (- ln rn))))
; 	(let ((w (car rhs))) (subseq w 0 (- rn ln))))))


(defun new-need (need word)
  (remove-if (lambda (c) (find c word)) need))

(defun new-rem (lhs rhs)
  (let ((ln (length lhs))
	(rn (length rhs)))
    (if (>= ln rn)
	(subseq lhs rn)
	(subseq rhs 0 (- rn ln)))))

(defun state-palindrome (state)
  (append (reverse (lhs state))
	  (rhs state)))

#|
(defmethod h-cost ((problem palindrome-problem) state) 
  "The estimated cost from state to a goal for this problem.  
   If you don't overestimate, then A* will always find optimal solutions.
   The default estimate is always 0, which certainly doesn't overestimate."
  (+ (1- (* 2 (length (need state)))) (abs (- (ln state) (rn state)))))

(defmethod h-cost ((problem palindrome-problem) state)
  (cache-h-cost state))
|#

; (defun make-action-state (state side word)
;   (let ((action (make-action :side side :word word)))
;     (cons action (apply-action state action))))

(defun combine-to-complete-p (even-seen odd-need)
  (every (lambda (c) (member c even-seen)) odd-need))

(defun init-seen (&rest word-lists)
  (let ((seen ()))
    (dolist (words word-lists seen)
      (dolist (word words)
	(loop for c across word do (pushnew c seen))))))

(defun init-need (x)
  (loop for c across *letters*
	unless (member c x)
	collect c))

(defun make-initial-state (&key lhs rhs)
  (let ((ln (words-length lhs))
	(rn (words-length rhs))
	(seen (init-seen lhs rhs)))
    (make-state :lhs lhs :rhs rhs :ln ln :rn rn
		:seen seen :need (init-need seen)
		:cache-h-cost 51)))

(defun sbcl-fail ()
  (with-open-file (out "/Users/Corey/out.log" :direction :output :if-exists :supersede)
    (loop for w across *words* for i from 0 do
	  (format out "~10d ~20a ~a~%" i w (has-word-ending (reverse w) *rwords*)) 
	  (finish-output out))))

(defun count-starting-words-worth-collecting ()
  (loop for word across *words*
	if (has-word-ending (reverse word) *rwords*)
	count word))

(defvar *swords*)

