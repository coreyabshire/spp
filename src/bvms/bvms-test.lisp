(defpackage :bvms-test
  (:use :common-lisp :bvms :cl-utils))

(in-package :bvms-test)

(defun bvms-string (bvms &optional (width 0))
  (format nil (format nil "~~~a,'0B" width) bvms))

(5am:test char-bvms
  (let* ((range (char-range #\a #\z))
	 (len (length range))
	 (bvms (make-bvms-gen range)))
    (flet ((fn (s) (bvms-string (funcall bvms s) len)))
      (5am:is (string= "00000000000000000000000000" (fn "")))
      (5am:is (string= "00000000000000000000000001" (fn "a")))
      (5am:is (string= "10000000000000000000000000" (fn "z")))
      (5am:is (string= "10000000000000000000000001" (fn "az")))
      (5am:is (string= "00000000000000000000000111" (fn "abc")))
      (5am:is (string= "00000000000000000000000111" (fn "aabbcc"))))))

(defbvms word-sig *letters* nil)

(defbvms word-sig *letters* t)

(time (loop for i from 1 to 100000 count (word-sig *letters*)))

