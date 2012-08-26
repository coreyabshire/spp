(in-package :spp)

(defun goalp (w)


(defun simple-h-cost (s)
  (let* ((b (word-sig s))
	 (c (logcount b))
	 (n (length s)))
    (+ (- (* (- 26 c) 2) 1) n)))

(defun find-it ()
  (find-it-nil 0 () 0))

; g - g-cost
; p - path so far

(defun find-it-try (g p bvms w lhs rhs)
  (let* ((g (+ g (length w)))
	 (p (cons w p))
	 (c (logcount bvms))
	 (h (+ 1 1)))

(defun find-it-nil (g b p)
  (if (find-it-goal g b)
      p
      (loop for w across *words* do
	    (find-it-try g p b w))))

(defun find-it-starting (g p b s)
  (do-words-starting (w (reverse s) *words* nil)
    (find-it-try g p b w)))

(defun find-it-ending (g p b s)
  (do-words-ending (w (reverse s) *rwords* nil)
    (find-it-try g p b w)))

(defun starting-test (s)
  (let ((c 0))
    (do-words-starting (w s *words* 'x)
      (if (> c 11)
	  (return w)
	  (incf c)))))


; if the current state is a goal, return the path that got you there
; otherwise, try all the words that are applicable to the current state
; at the next level of depth.

(defstruct state diff ln rn lhs rhs bvms f-limit g depth)

(defun apply-action (state action)
  (let ((lhs (or (lhs state) action))
	(rhs (or (rhs state) action)))
    (let ((ln (length lhs))
	  (rn (length rhs))
	  (bvms (logior (word-sig action) (bvms state))))
      (cond ((= ln rn) (make-state :bvms bvms))
	    ((> ln rn) (make-state :bvms bvms :lhs (subseq lhs rn)))
	    ((< ln rn) (make-state :bvms bvms :rhs (subseq rhs 0 (- rn ln))))))))

(defun newdiff (diff side word len)
  ;; which side 
  (if (eq side 'lhs)
      (if (>= (len diff) len)
	  (subseq diff 
      len))

(defun find-it (words rwords &optional diff ln rn lhs rhs bvms f-limit g depth)
  ;; check if the current diff and bvms tells us its the goal
  (if (and (= bvms *goal-sig*) (string= diff (reverse diff)))
      ;; if it is, return the path (as a joined string of words from lhs and rhs)
      (values (format nil "~{~a~^ ~}" (append lhs (reverse rhs))) g)
      ;; otherwise...
      ;; determine which side gets the new word
      (let* ((side (if (<= ln rn) 'lhs 'rhs))
	     (op (if (eq side 'lhs) 'starts 'ends))
	     (seq (if (eq op 'starts) words rwords))
	     (s (reverse diff)))
	;; for each word that we could add to the other side
	(loop with fn = (position-iterator s seq op)
	      for pos = (funcall fn)
	      for word = (aref seq pos)
	      ;; determine the cost of adding the word
	      for len = (length word)
	      for newg = (+ g len)
	      ;; determine the new h-cost
	      for sig = (word-sig word)
	      for newbvms = (logior bvms sig)
	      for newdiff = (newdiff 
	      for h-cost = (+  (logcount newbvms))
	      ;; determine the new f-cost
	      
	      ;; determine whether f-cost is still under f-limit
	      ;; calculate the new difference
	      ;; calculate the new lengths of the sides
	      ;; add the word to one of the sides
      )))))

(defun find-it-loop (words rwords &optional diff ln rn lhs rhs bvms f-limit g)
  (if (<= ln rn)
      (find-it-lhs (words rwords diff ln rn lhs rhs bvms f-limit g))
      (find-it-rhs (words rwords diff ln rn lhs rhs bvms f-limit g))))

(defun find-it

      (cond (lhs (word-loop where word ends with (reverse lhs) in words
	       (let* ((s (or lhs rhs))
		      (b (logior bvms (word-sig w)))
		      (c (logcount b))
		      (h (+ (- (* (- 26 c) 2) 1) (length s)))
		      (f (+ g h)))
		 (if (and (string= s (reverse s)) (= c 26))
		     (format t "GOAL: ~a~%" path)
		     (if (< f f-limit)
			 (find-it words rwords lhs rhs bvms f-limit g ))))))))
		     
		 
		      

;	(rhs (loop for (action . delta) in (actions-starting problem rhs) do
;		   (push (cons action (combine-states state delta)) successors)))
;	(t   (loop for (action . delta) in (actions-starting problem "") do
;		   (push (cons action (combine-states state delta)) successors))))

