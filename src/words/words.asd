;; -*- lisp -*-

(asdf:defsystem :words
  :components ((:file "package")
	       (:file "read" :depends-on ("package"))
	       (:file "search" :depends-on ("package")))
  :depends-on (:util :bsearch :bvms))

