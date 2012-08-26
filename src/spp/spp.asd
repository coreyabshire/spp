;; -*- lisp -*-

(asdf:defsystem :spp
  :components ((:file "package")
	       (:file "data" :depends-on ("package"))
	       (:file "testdata" :depends-on ("package"))
	       (:file "spcost" :depends-on ("package"))
	       (:file "network" :depends-on ("package"))
	       (:file "sppnet" :depends-on ("package"))
	       (:file "analysis" :depends-on ("package")))
  :depends-on (:util
	       :bsearch
	       :bvms
	       :words
	       :aima
	       :split-sequence
	       :fiveam))

