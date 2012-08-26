(in-package :palindrome)

; find the shortest sequence of words from the given list of words that is both
; and palindrome and a pangram

; find the shortest set of WORDS that has all of LETTERS
; and that is a palindrome

; strategy:
;  - given a word, find all possible palindromes starting with that word
;  - what are the problem words
;  - need to identify some test case words
;  - develop a strategy for shortcutting the search

(defun next-palindromes (dict word)
  "Return a list of the left right combinations that are potential palindromes
having the given word as the first word on the left."
  (let ((result ()))
    (do-words-ending (w (reverse word) dict)
      (push (cons word w) result))
    (nreverse result)))

; bob | kabob

(defun palindrome (dict left right)
  (let ((current (append left right)))
;    (format t "     ~a | ~a : ~a~%" left right (diff-lists left right))
;    (format t "     ~a~%" (diff-lists left right))
    (finish-output)
    (cond ((palindromep current) current)
	  ((and (not (null left)) (not (null right))
		(string= (first (last left)) (first right))) nil)
	  ((and (> (length right) 2) (equal (car right) (cadr right))) nil)
	  ((and (not (null right)) (member (first right) (rest right) :test #'string=)) nil)
	  ((and (not (null left)) (let ((x (reverse left))) (member (first x) (rest x)))) nil)
	  (t (let ((ls (apply #'concatenate 'string left))
		   (rs (apply #'concatenate 'string right)))
	       (let ((ln (length ls))
		     (rn (length rs)))
		 (if (> ln rn)
		     (let ((diff (subseq ls rn)))
		       (do-words-ending (w (reverse diff) dict)
			 (let ((b (palindrome dict left (cons w right))))
			   (if b (format t "~a~%" b)))))
		     (let ((diff (subseq rs 0 (- rn ln))))
		       (do-words-starting (w (reverse diff) dict)
			 (let ((b (palindrome dict (append left (list w)) right)))
			   (if b (format t "~a~%" b))))))))))))

