;;;; Program to find shortest palindromic pangram. - Corey Abshire

(in-package :palindrome)

(defvar *dict* (load-dictionary "/Users/Corey/ita/palindrome/data/WORD.LST"))  

(setf *print-right-margin* 1024)

