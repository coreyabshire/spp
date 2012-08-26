;; -*- lisp -*-

(asdf:defsystem :aima
  :components ((:file "packages")
	       (:file "utilities" :depends-on ("packages"))
	       (:file "queue" :depends-on ("packages"))
	       (:file "problem" :depends-on ("packages" "utilities" "queue"))
	       (:file "simple" :depends-on ("packages" "utilities" "queue"))
	       (:file "repeated" :depends-on ("packages" "utilities" "queue"))
	       (:file "sma" :depends-on ("packages" "utilities" "queue"))
	       (:file "ida" :depends-on ("packages" "utilities" "queue"))
	       (:file "iterative" :depends-on ("packages" "utilities" "queue"))))

