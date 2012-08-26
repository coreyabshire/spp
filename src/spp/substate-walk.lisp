;;;; Walk all possible substates for the given word list.

(in-package :spp)

(defparameter *next-uid* 0)

(defun next-uid ()
  (incf *next-uid*))

(defstruct substate
  uid
  lhs
  rhs
  successors
  predecessors
  pal?
  deadend?)

(defvar *states*)

(defparameter *root* (make-substate :uid (next-uid)))

(defvar *lhs-states*)

(defvar *rhs-states*)

(defun hash-values (h)
  (loop for v being the hash-values of h collect v))

(defun all-substates ()
  (append (list *root*)
	  (hash-values *lhs-states*)
	  (hash-values *rhs-states*)))

(defun all-substates-by-uid ()
  (sort (all-substates) #'< :key #'substate-uid))

(defmethod print-object ((object substate) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":uid ~a :lhs ~a :rhs ~a :suc ~a :pre ~a :pal? ~a :deadend? ~a"
	    (substate-uid object)
	    (substate-lhs object)
	    (substate-rhs object)
	    (length (substate-successors object))
	    (length (substate-predecessors object))
	    (substate-pal? object)
	    (substate-deadend? object))))

(defun init-substates ()
  (setq *next-uid* 0)
  (setq *root* (make-substate :uid (next-uid) :pal? t))
  (setq *lhs-states* (make-hash-table :test #'equal))
  (setq *rhs-states* (make-hash-table :test #'equal))
  (expand-substate *root*)
  (mark-deadends)
  (loop for c = (mark-more-deadends) while (> c 0))
  ;(remove-deadends)
  'ready)

(defun string-pal? (s)
  (string= s (reverse s)))

(defun lhs-substate (lhs)
  (multiple-value-bind (val hit) (gethash lhs *lhs-states*)
    (if hit
	val
	(setf (gethash lhs *lhs-states*)
	      (make-substate :uid (next-uid) :lhs lhs
			     :pal? (string-pal? lhs))))))

(defun rhs-substate (rhs)
  (multiple-value-bind (val hit) (gethash rhs *rhs-states*)
    (if hit
	val
	(setf (gethash rhs *rhs-states*)
	      (make-substate :uid (next-uid) :rhs rhs
			     :pal? (string-pal? rhs))))))

(defun next-substate (substate word)
  (let ((lhs (or (substate-lhs substate) word))
	(rhs (or (substate-rhs substate) word)))
    (let ((ln (length lhs))
	  (rn (length rhs)))
      (cond ((= ln rn) *root*)
	    ((> ln rn) (lhs-substate (subseq lhs rn)))
	    ((< ln rn) (rhs-substate (subseq rhs 0 (- rn ln))))))))

(defun compute-substate-successors (substate)
  (with-accessors ((lhs substate-lhs) (rhs substate-rhs)) substate
    (cond (lhs (loop for w in (words-ending (reverse lhs) *rwords*)
		  collect (cons w (next-substate substate w))))
	  (rhs (loop for w in (words-starting (reverse rhs) *words*)
		  collect (cons w (next-substate substate w))))
	  (t (loop for w across *words*
		collect (cons w (lhs-substate w)))))))
	       
(defun expand-substate (substate)
  (setf (substate-successors substate) (compute-substate-successors substate))
  (loop for (word . successor) in (substate-successors substate)
     do (push substate (substate-predecessors successor)))
  (loop for (word . successor) in (substate-successors substate)
     unless (substate-successors successor)
     do (expand-substate successor)))

(defun print-substates ()
  (format t "~a~%" *root*)
  (loop for v being the hash-values of *lhs-states*
     do (format t "~a~%" v))
  (loop for v being the hash-values of *rhs-states*
     do (format t "~a~%" v)))
       

(defun mark-deadends ()
  (loop for s in (all-substates)
     if (and (= 0 (length (substate-successors s)))
	     (not (substate-pal? s)))
     do (setf (substate-deadend? s) t)))

(defun extended-deadend (s)
  (and (not (substate-pal? s))
       (substate-deadend? s)))

(defun remove-deadends ()
  (loop for s in (all-substates)
     do (setf (substate-successors s) (remove-if #'substate-deadend? (substate-successors s) :key #'cdr))))

(defun mark-more-deadends ()
  (loop with c = 0
     for s in (all-substates)
     if (and (every #'extended-deadend (mapcar #'cdr (substate-successors s)))
	     (not (substate-pal? s)))
     do (progn (when (not (substate-deadend? s))
		 (setf (substate-deadend? s) t)
		 (incf c)))
     finally (return c)))


(defun combine-substate (state substate word)
  "Given a delta as returned from actions-ending or actions-starting,
  combines it with the current state to form a new state."
  (make-state :lhs (substate-lhs substate)
	      :rhs (substate-rhs substate)
	      :bvms (logior (word-bvms word)
			    (bvms state))))

(defmethod successors ((problem palindrome-problem) state)
  "Return a list of (action . state) pairs. An action is a pair of the
  form (side . word), where side represents the side to add the word to.
  The state is effectively a string representing the remaining letters."
  (with-accessors ((lhs lhs) (rhs rhs)) state
    (let ((s ()))
      (cond (lhs (loop for (action . substate) in (substate-successors (gethash lhs *lhs-states*))
		    do (push (cons action (combine-substate state substate action)) s)))
            (rhs (loop for (action . substate) in (substate-successors (gethash rhs *rhs-states*))
		    do (push (cons action (combine-substate state substate action)) s)))
            (t   (loop for (action . substate) in (substate-successors *root*)
		    do (push (cons action (combine-substate state substate action)) s))))
      s)))

(defun print-csv ()
  (with-open-file (out "/home/corey/substates-vlist.csv"
		       :direction :output
		       :if-exists :supersede)
    (format out "~a,~a,~a,~a,~a,~a,~a,~a~%"
	    "deadend" "pal" "type" "rem" "uid" "nsucc" "npred" "len")
    (loop for s in (all-substates-by-uid) do
      (format out "~a,~a,~a,~a,~a,~a,~a,~a~%"
		  (if (substate-deadend? s) 1 0)
		  (if (substate-pal? s) 1 0)
		  (if (substate-lhs s) "LHS" (if (substate-rhs s) "RHS" "ROOT"))
		  (if (substate-lhs s) (substate-lhs s) (substate-rhs s))
		  (substate-uid s)
		  (length (substate-successors s))
		  (length (substate-predecessors s))
		  (length (if (substate-lhs s) (substate-lhs s) (substate-rhs s)))
	      )))
  (with-open-file (out "/home/corey/substates-edgelist.csv"
		       :direction :output
		       :if-exists :supersede)
    (format out "~a,~a,~a~%" "from" "to" "action")
    (loop for s in (all-substates-by-uid) do
      (loop for (a . c) in (substate-successors s)
	    ;unless (substate-deadend? c)
	    do (format out "~a,~a,~a~%"
			   (substate-uid s)
			   (substate-uid c)
			   a
		       ))))
)

(defun update-csv (basename)
  (init (read-words (data-file basename)))
  (init-substates)
  (print-csv)
  'done)