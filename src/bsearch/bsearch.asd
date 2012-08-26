;; -*- lisp -*-

(defsystem :bsearch
    :author "Corey Abshire <corey.abshire@gmail.com>"
    :components ((:file "package")
		 (:file "bsearch" :depends-on ("package"))))

