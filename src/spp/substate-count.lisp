;;;; Count substate usage.

(in-package :spp)

(defun make-substate-counts ()
  (make-hash-table :test 'equalp))

(defvar *substate-counts* (make-substate-counts))

(defun reset-substate-counts ()
  (setq *substate-counts* (make-substate-counts)))

(defun collect-state (state)
  (incf (gethash (make-substate-from-state state) *substate-counts* 0)))

(defmethod successors :around ((problem palindrome-problem) state)
  (let ((results (call-next-method)))
    (loop for (action . state) in results
       do (collect-state state))
    results))

(defun hash-keys (h)
  (loop for k being the hash-keys of h collect k))

(defun substate-order-desc (a b)
  (> (gethash a *substate-counts*)
     (gethash b *substate-counts*)))

(defun substates-by-frequency ()
  (sort (hash-keys *substate-counts*) #'substate-order-desc))

(defun top-n-substates (n)
  (let ((s (substates-by-frequency)))
    (subseq (substates-by-frequency) 0 (min (length s) n))))

(defun print-top-n-substates (n)
  (loop for substate in (top-n-substates n)
     do (format t "LHS: ~a RHS: ~a COUNT: ~a~%"
		(substate-lhs substate)
		(substate-rhs substate)
		(gethash substate *substate-counts*))))

(defun disable-substate-count ()
  (let ((generic #'successors))
    (unwind-protect
	 (let ((method (find-method generic '(:around) '(palindrome-problem t))))
	   (remove-method generic method)))))
