
(defpackage :bsearch-test
  (:use :common-lisp :bsearch :fiveam))

(in-package :bsearch-test)

(test bs-position-if
  "Test bs-position-if."
  (let ((v (vector "c" "f" "g")))
    (is (= 0  (bs-position-if "c" v)))
    (is (null (bs-position-if "d" v)))
    (is (null (bs-position-if "b" v)))
    (is (= 2  (bs-position-if "g" v)))
    (is (null (bs-position-if "h" v))))
  (let ((v (vector "a" "a" "a" "b")))
    (is (= 0 (bs-position-if "a" v))))
  (let ((v (vector "abc" "bce")))
    (is (= 0 (bs-position-if "a" v)))
    (is (= 0 (bs-position-if "ab" v)))
    (is (= 0 (bs-position-if "abc" v)))
    (is (null (bs-position-if "ac" v)))
    (is (= 1 (bs-position-if "b" v)))
    (is (= 1 (bs-position-if "bc" v)))
    (is (null (bs-position-if "bcd" v)))
    (is (= 1 (bs-position-if "bce" v)))))

(test binary-search
  (let ((v (vector "a" "b" "c")))
    (is (= 0 (binary-search "a" v)))
    (is (= 1 (binary-search "b" v)))
    (is (= 2 (binary-search "c" v)))
    (is (null (binary-search "d" v)))))

