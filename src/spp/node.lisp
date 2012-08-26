(in-package :palindrome)

;; states: a state is a set of words, separated into the left and 
;;         right. the words must form a partial palindrome.
;; initial state: any word can be designated as the initial state
;; successor function: generates a set of potential palindromes
;;                     by finding all the words that match based
;;                     on the remainder of the left over letters.
;; goal test: checks whether the state represents a complete
;;            palindrome and whether it uses all 26 letters of the
;;            alphabet
;; path cost: represents the number of unnecessary letters used
;;            in generating the palindrome

(defclass node ()
  ((state :initarg :state)
   (parent :initarg :parent)
   (action :initarg :action)
   (path-cost :initarg :path-cost)
   (depth :initarg :depth)))


(defun initial-state ()
  (make-instance 'node
		 :state ""
		 :parent nil
		 :action nil
		 :path-cost nil
		 :depth 0))
		 

(defun successors (node dict)
  ; if lhs heavy
  ;   find words in dict that end with reverse diff
  ;   append each such word to the opposite side to form a successor
  ; else
  ;   find words in dict that start with reverse diff
  ;   append each such word to the opposite side to form a successor

(def-print-object-with-slots node state parent action path-cost depth)