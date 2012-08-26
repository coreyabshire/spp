;; -*- lisp -*-

(defpackage :palindrom.system
  (:use :common-lisp
	:asdf))

(in-package :palindrome.system)

(defsystem :palindrome
  :author "Corey Abshire <corey.abshire@gmail.com>"
  :properties ((:test-suite-name . :palindrome))
  :components ((:static-file "palindrome.asd")
	       (:module :src
			:components ((:file "utils")
				     (:file "bsearch" :depends-on ("utils"))
				     (:file "pp" :depends-on ("bsearch" "utils"))))
	       (:module :test
			:components ((:file "bsearch-test")
				     (:file "pp-test"))
			:depends-on (:src))))
				     
