(in-package   :aima)
(5am:def-suite aima)
(5am:in-suite  aima)

(5am:test basic-queue-operations
  (let ((q (make-empty-queue)))
    ;; Throw a few items in the queue, checking
    ;; whether its empty or not as would expect
    ;; as we go along.
    (5am:is-true (empty-queue? q))
    (5am:is (not (null (enqueue-at-end q '(a)))))
    (5am:is-false (empty-queue? q))
    (5am:is (not (null (enqueue-at-end q '(b)))))
    (5am:is (not (null (enqueue-at-end q '(c)))))
    (5am:is-false (empty-queue? q))
    ;; Now remove the items from the queue, continuing
    ;; to check whether its empty and also checking to
    ;; make sure the queue-front behaves as expected.
    (5am:is (eq (queue-front q) 'a))
    (5am:is (eq (queue-front q) 'a))
    (5am:is (eq (remove-front q) 'a))
    (5am:is (not (eq (queue-front q) 'a)))
    (5am:is (eq (queue-front q) 'b))
    (5am:is-false (empty-queue? q))
    (5am:is (eq (remove-front q) 'b))
    (5am:is (eq (remove-front q) 'c))
    (5am:is-true (empty-queue? q))
    ;; Now try the same basic thing, but mix a call to
    ;; remove in the middle of an enqueue function and
    ;; see if it still behaves as expected.
    (5am:is (not (null (enqueue-at-end q '(d)))))
    (5am:is (not (null (enqueue-at-end q '(e)))))
    (5am:is (eq (remove-front q) 'd))
    (5am:is (not (null (enqueue-at-end q '(f)))))
    (5am:is (eq (remove-front q) 'e))
    (5am:is (eq (remove-front q) 'f))
    (5am:is-true (empty-queue? q))
    ))

(defstruct ti v p)

(5am:test priority-queue-operations
  (let ((q (make-empty-queue)))
    ;; Throw a few items in the queue, checking
    ;; whether its empty or not as would expect
    ;; as we go along.
    (5am:is-true (empty-queue? q))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'tca :p 8)) #'ti-p)))
    (5am:is-false (empty-queue? q))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'mra :p 5)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'dca :p 31)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'mb :p 10)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'mjb :p 29)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'ab :p 6)) #'ti-p)))
    (5am:is-false (empty-queue? q))
    (5am:is (eq 'mra (ti-v (queue-front q))))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'jj :p 30)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'la :p 2)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'ha :p 30)) #'ti-p)))
    (5am:is (null (enqueue-by-priority q (list (make-ti :v 'na :p 31)) #'ti-p)))
    (5am:is-false (empty-queue? q))
    (5am:is (eq 'la (ti-v (queue-front q))))
    (5am:is-false (empty-queue? q))
    (5am:is (eq 'la (ti-v (remove-front q))))
    (5am:is (eq 'mra (ti-v (remove-front q))))
    (5am:is (eq 'ab (ti-v (remove-front q))))
    (5am:is (eq 'tca (ti-v (queue-front q))))
    (5am:is (eq 'tca (ti-v (remove-front q))))
    (5am:is (eq 'mb (ti-v (remove-front q))))
    (5am:is (eq 'mjb (ti-v (remove-front q))))
    (5am:is (eq 'jj (ti-v (remove-front q))))
    (5am:is (eq 'ha (ti-v (remove-front q))))
    (5am:is (eq 'na (ti-v (remove-front q))))
    (5am:is (eq 'dca (ti-v (queue-front q))))
    (5am:is-false (empty-queue? q))
    (5am:is (eq 'dca (ti-v (remove-front q))))
    (5am:is-true (empty-queue? q))
    ))
  
