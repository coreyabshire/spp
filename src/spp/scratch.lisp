(let ((a (make-state :lhs "abc"))
      (b (make-state :lhs "abc")))
  (equalp a b))