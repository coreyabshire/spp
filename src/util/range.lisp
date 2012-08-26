(in-package :util)

(defun range (a b)
  "Generate a list of numbers from A to B."
  (loop for i from a upto b collect i))

(defun char-range (a z)
  (map 'string #'code-char (range (char-code a) (char-code z))))

