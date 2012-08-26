;; -*- lisp -*-

(asdf:defsystem :wordset
  :components ((:file "package")
	       (:file "wordset" :depends-on ("package")))
  :depends-on (:util :words))

