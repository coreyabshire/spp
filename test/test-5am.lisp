
(defpackage :dom7b5.5am.test
  (:use :common-lisp :it.bese.FiveAM))

(in-package :dom7b5.5am.test)

(defun my-add (&rest numbers)
  (apply #'+ numbers))

(defun dummy-add (a b)
  (+ a b))

(defun dummy-strcat (a b)
  (concatenate 'string a b))

(def-suite example-suite
    :description "The example test suite.")

(in-suite example-suite)

(test my-add
  (is (= 0 (my-add 2 -2)))
  (is (= 1 (my-add .5 .5)))
  (is (= 4 (my-add 2 2)))
  (is (= 3 (my-add 2 5))))

(test add-2
  "Test the MY-ADD function"
  (is (= 2 (my-add 1 1)))
  (is (= 0 (my-add -2))))

(test dummy-add
  (for-all ((a (gen-integer))
	    (b (gen-integer)))
    (is (= (+ a b) (dummy-add a b)))
    (is (= (dummy-add a b)
	   (dummy-add b a)))
    (is (= a (dummy-add a 0)))
    (is (= 0 (dummy-add a (- a))))
    (is (< a (dummy-add a 1)))
    (is (= (* 2 a) (dummy-add a a)))))

(test dummy-strcat
  (for-all ((result (gen-string))
	    (split-point (gen-integer :min 0 :max 10000)
			 (< split-point (length result))))
    (is (string= result (dummy-strcat (subseq result 0 split-point)
				      (subseq result split-point))))))

(test random-failure
  (for-all ((result (gen-integer :min 0 :max 1)))
    (is (plusp result))
    (is (= result 0))))
