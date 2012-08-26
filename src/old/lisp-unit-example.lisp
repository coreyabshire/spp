(defpackage :lisp-unit-examples
  (:use :common-lisp
	:lisp-unit))

(in-package :lisp-unit-examples)

(define-test pick-greater
  (assert-equal 5 (pick-greater 2 5))
  (assert-equal 5 (pick-greater 5 2))
  (assert-equal 10 (pick-greater 10 10))
  (assert-equal 0 (pick-greater -5 0)))

(defun pick-greater (x y)
  x)
