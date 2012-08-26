(defpackage :bsearch-test
  (:use :common-lisp :bsearch :5am))

(in-package :bsearch-test)

(def-suite bsearch-test)

(in-suite bsearch-test)

(test bsearch
  (let ((names (vector "caleb" "corey" "rosie")))
    (is (null (bs-position-if "billy" names)))
    (is (= 0  (bs-position-if "caleb" names)))
    (is (= 1  (bs-position-if "corey" names)))
    (is (null (bs-position-if "corex" names)))
    (is (= 2  (bs-position-if "rosie" names)))
    (is (null (bs-position-if "willy" names)))
    ))


