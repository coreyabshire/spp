(defpackage :words
  (:use :common-lisp :bsearch :util)
  (:export #:read-words
	   #:make-words-from-list
	   #:make-words-from-file
	   #:words-forward
	   #:words-reverse
	   #:do-words-starting
	   #:do-words-ending
	   #:has-word-starting
	   #:has-word-ending
	   #:words-starting
	   #:words-ending
	   #:word-loop
	   #:position-iterator))

