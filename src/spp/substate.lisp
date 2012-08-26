;;;; Count substate usage.

(in-package :spp)

(defstruct substate lhs rhs)

(defun make-substate-from-state (state)
  (make-substate :lhs (lhs state) :rhs (rhs state)))

