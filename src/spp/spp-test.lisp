(in-package :spp)

(defun test-find-spp (wordlist length)
  (let* ((solution (find-spp :wordlist wordlist :heuristic 'simple :use-worth-collecting-filter nil))
	 (s (remove #\Space solution)))
    (5am:is (string= s (reverse s)) "not a palindrome")
    (5am:is (every (lambda (c) (find c s)) (map 'list #'identity *letters*)) "not a pangram")
    (5am:is (= length (length s)) "length check failed: expected ~a, actual ~a" length (length s))))
  
(5am:test bug-palindrome-ends-in-middle
  (test-find-spp '("abc" "defghijk" "lmnop" "qrstu" "vwxy" "zyxwvutsrqp" "onmlkji" "hgfedcba") 51))

(5am:test single-letter-words
  (test-find-spp (map 'list #'string *letters*) 51))

(5am:test find-spp
  (test-find-spp '("abc" "defghijk" "lmnop" "qrstu" "vwxy" "zyxwvutsrqp" "onmlkji" "hgfedcba") 51)
  (test-find-spp '("abxc" "defghijk" "lmnop" "qrstu" "vwxy" "zyxwvutsrqp" "onmlkji" "hgfedcxba") 53)
  (test-find-spp '("abc" "defghijk" "lmnop" "qrstu" "vwxyzabc" "cbazyxwvutsrqp" "onmlkji" "hgfedcba") 58)
  (test-find-spp (map 'list #'string *letters*) 51))
