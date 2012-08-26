(in-package :aima)

;;;; Debugging tool

(defvar *debugging* nil)

(defvar *basepath* nil)

(defvar *logfile* nil)

(defvar *log-run-id* 0)

(defvar *noisy*)

(defun enable-debugging (&key (enable t))
  (setf *debugging* enable)
  (setf *noisy* enable))

(defun log-file (basename)
  (format nil "~a/logs/~a.log" *basepath* basename))

(defun timestamp ()
  (multiple-value-bind (s mi h d m y) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d" y m d h mi s)))

(defun timestamp-log-filename ()
  (format nil "/home/corey/Projects/SPP/logs/spp-~a.log" (timestamp)))

(defun open-run-log ()
  (open-log (format nil "run-~5,'0d" (incf *log-run-id*))))

(defmacro with-run-log (&body body)
  `(progn (setf *logfile* (open (timestamp-log-filename)
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
		aima::*debugging* t
		aima::*noisy* t
		*print-right-margin* 4096)
	  (unwind-protect (progn ,@body)
	    (close-log))))

(defun open-log (basename)
  (setf *logfile* (open (log-file basename)
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)))

(defun set-log-id (id)
  (setf *log-run-id* id))

(defun set-base-path (basepath)
  (setf *basepath* basepath))

(defun close-log ()
  (close *logfile*)
  (setf *logfile* nil))

(defmacro write-log (control-string &rest arguments)
  `(format (or *logfile* t) ,control-string ,@arguments))

(defun dprint (&rest args)
  "Echo all the args when *debugging* is true.  Return the first one."
  (when *debugging* (format t "~&~{~S ~}~%" args))
;  (when *debugging* (write-log "~&~{~S ~}~%" args))
  (first args))

(defconstant infinity most-positive-single-float)

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))




(defun the-smallest (fn l)
  (the-biggest (compose #'- fn) l))

(defun the-smallest-random-tie (fn l)
  (the-biggest-random-tie (compose #'- fn) l))

(defun the-smallest-that (fn p l)
  (the-biggest-that (compose #'- fn) p l))

(defun compose (f g)
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(defun the-biggest (fn l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
	(when (> val best-val)
	  (setq best-val val)
	  (setq biggest x))))
    biggest))

(defun the-biggest-random-tie (fn l)
  (random-element
   (let ((biggest (list (first l)))
	 (best-val (funcall fn (first l))))
     (dolist (x (rest l))
       (let ((val (funcall fn x)))
	 (cond ((> val best-val)
		(setq best-val val)
		(setq biggest (list x)))
	       ((= val best-val)
		(push x biggest)))))
     biggest)))

(defun the-biggest-that (fn p l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (when (funcall p x)
	(let ((val (funcall fn x)))
	  (when (> val best-val)
	    (setq best-val val)
	    (setq biggest x)))))
    biggest))

