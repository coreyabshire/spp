(in-package :util)

;(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)) logdist)
;	 (inline logdist))

(defmacro make-logdist (sig)
  (let ((bits (gensym)))
    `(case (logcount ,sig)
       ,@(loop for c from 2 below 5
	      for syms = (loop for i from 0 below c collect (gensym))
	      collect `(,c (let ((,bits (bits ,sig)))
			     (let ,(loop for sym in syms for i from 0 collect `(,sym (nth ,i ,bits)))
			       (lambda (n)
				 (declare (fixnum n ,@(loop for sym in syms collect sym)))
				 (logior ,@(loop for sym in syms for i from 0 collect `(ash (ldb (byte 2 ,i) n) ,sym)))))))))))

(defun test-logdist-3 (sig)
  (loop with fn = (make-logdist sig)
	for n from 0 below (expt 2 3)
	for d = (funcall fn n)
	do (format t "~4d ~26,'0B~%" n d)))
#|
(defmacro make-logdist (sig)
  (let ((n (gensym))
	(esig (gensym)))
    `(let ((,esig sig))
       (lambda (,n)
	 (logior ,(loop with j = 0
			for i from 0 below 25
			if (logbitp i esig)
			collect `(ldb (byte 2 ,i) (logbitp ,n))))))))
|#

(defun logdist-5 (n bits)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
	   ((unsigned-byte 32) n))
  (loop with d of-type fixnum = 0
	for pos of-type fixnum in bits
	for c of-type fixnum from 0
	if (logbitp c n)
	do (setf d (logior d pos))
	finally (return d)))

(defun bits (sig)
  (loop for i from 0 below 26
	if (logbitp i sig)
	collect i))

(defun logdist2 (n bits)
  (loop with d = 0
	with c = -1
	for b in bits
	do (format t "~a~%" b)))

(defun logdist2-test (sig)
  (loop with bits = (bits sig)
	for n from 0 to (expt 2 (logcount sig))
	do (format t "~4d: ~26,'0B ~%" n (logdist2 n bits))))

(defun logdist (n mask)
;  (declare (optimize (compliation-speed 0) (debug 0) (safety 0) (space 0) (speed 3))
;	   ((unsigned-byte 32) n mask))
  (let ((d 0)
	(c 0))
    (declare ((unsigned-byte 32) d c))
    (dotimes (i 26 d)
      (declare ((unsigned-byte 32) i))
      (if (logbitp i mask)
	  (progn (if (logbitp c n)
		     (setf (ldb (byte 2 i) d) 1))
		 (incf c))))))

(defun logdist-test (n sig)
  (loop for i from 1 upto n
	for a = (logdist i sig)
	for b = (logxor a sig)
	do (format t "~26,'0B ~26,'0B ~26,'0B ~26,'0B ~2d ~2d ~2d ~%" sig i a b i a b)))

(defun mask-loop-logdist (sig)
  (logdist-test (1- (expt 2 (logcount sig))) sig))

(defparameter *nletters* 26)

(defparameter *bits*
  (loop for n from 0 to *nletters*
	for i = 0
	do (setf (ldb (byte 2 n) i) 1)
	collect i))
	

(defun mask-loop-test (sig)
  (let ((bits (split-sig sig))
	(a 0)
	(b sig))
    (loop while bits
	  do (progn (loop for bit in bits
			  do (progn (setq a (logxor bit a) b (logxor bit b))
				    (format t "~26,'0B ~26,'0B~%" a b)
				    (setq a (logxor bit a) b (logxor bit b))))
		    (let ((bit (pop bits)))
		      (setq a (logxor bit a) b (logxor bit b)))))))

