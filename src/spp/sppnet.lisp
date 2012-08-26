;;;; -*- Mode: Lisp; -*- 
;;;;
;;;; Shortest Palindromic Pangram Problem Domain (ITA) - dca
;;;;

;;;; ----------------------------------------------------------------
;;;;
;;;;  There are three packages related to the SPP problem directly.
;;;;  
;;;;  SPP      - This is the main package, that contains the full
;;;;             problem definition in the AIMA search problem
;;;;             framework, plus the calls to all the other libraries
;;;;             and utilities, plus the interface function
;;;;             implementations for SPP-USER.
;;;;
;;;; ----------------------------------------------------------------

(in-package :spp)

;;;; ----------------------------------------------------------------
;;;;
;;;;  Parameters, Specials, and Constants
;;;;
;;;; ----------------------------------------------------------------

(defparameter *basepath* "/home/corey/Projects/SPP"
  "This is the base path wherever the other data SPP project
  files, including the data files, are located.")

(defun data-file (basename)
  "Turn the given basename into the path to a datafile."
  (format nil "~a/data/~a.LST" *basepath* basename))

(defparameter *filespec* (data-file 'word)
  "This is the main file from ITA that has the word list to find
  the shortest palindrome pangram from.")

(defparameter *letters* "abcdefghijklmnopqrstuvwxyz"
  "A string of all the lower case letters of the alphabet.")

(defparameter *nletters* (length *letters*)
  "The number of letters in the alphabet.")

;;;; construct a new word bvms generator for the
;;;; alphabet
(defbvms word-bvms *letters* t)

(defparameter *goal-bvms* (word-bvms *letters*)
  "The word-bvms of the list of all the words that we will get
  when the list of words comprises a full pangram.")

(defvar *last-node*)

(defvar *successor-calls* 0)

(defvar *successor-states* 0)

;;;; ----------------------------------------------------------------
;;;;
;;;;  Problem and State Structures
;;;;
;;;;  The AIMA presents search problems using a generic search
;;;;  problem framework. This framework generalizes the notion of
;;;;  a search problem into an initial state, a method of finding
;;;;  all the successors of a given state, a method to test
;;;;  whether a given state is a goal, and methods for determing
;;;;  the cost of each action taken to get to a state.
;;;;
;;;;  This section defines the state and problem structures
;;;;  for the shortest palindrome pangram problem domain.
;;;;
;;;;  The overall problem structure roughly corresponds to the
;;;;  list of words we are to use to construct the palindrome. It 
;;;;  also contains any other required caches or settings that can
;;;;  be applied to the problem as a whole.
;;;;
;;;;  A state roughly corresponds to a partial palindrome in
;;;;  progress. Logically it has a left side and a right side.
;;;;  However, as a memory optimization, we don't actually need
;;;;  to store all the words for the left side and the right side.
;;;;  That list can be rebuilt based on the set of actions taken
;;;;  once a solution is found. The LHS and RHS actually mentioned
;;;;  in the structure instead represent the leftover portion of
;;;;  the palindrome, on whichever side is currently longer. This
;;;;  is the set of characters that do not currently have a match
;;;;  on the opposing side.
;;;;
;;;;  The other aspect of the state is the set of letters that the
;;;;  current path has acquired thus far. It is represented as a
;;;;  bit vector, where each bit represents whether one letter from
;;;;  the alphabet has been seen yet by this state or not.
;;;;
;;;; ----------------------------------------------------------------

(defstruct state
  "A state in this search space is a combination of any unmatched 
  characters for the current palindrome under construction, plus
  a BVMS (bit-vector member summary) representing the letters
  that have been matched thus far towards having a complete
  palindrome. A goal state is found when the unmatched characters
  are themselves a palindrome, and the bit-vector is all 1's.
  Each action is the word added to this state to derive a new
  state. Thus, the path to the goal state represents the set of
  words that comprise the ultimate response to the challenge."
  vertex
  (bvms 0 :type fixnum))

(defstruct (spp-problem
	     (:include problem (initial-state (make-state)))
	     (:conc-name nil))
  "An overall definition of the problem of finding the shortest
  pangram palindrome. This structure is used to hold all the various
  word lists and caches, and also has the methods called by the
  AIMA search framework. (successors, goal-test, h-cost, etc...)"
  (heuristic 'simple)
  network
  (letters *letters*)
  (use-worth-collecting-filter nil)
  (use-logging nil)
  (successor-calls 0)
  (successor-call-limit 2000))

;;;; ----------------------------------------------------------------
;;;;
;;;;  Goal Test
;;;;
;;;;  The goal in this problem is to find the shortest list of words
;;;;  that is both a palindrome and a pangram. The goal-test within
;;;;  the AIMA problem framework can forget about whether its the
;;;;  shortest or not. It just needs to make sure that the other two
;;;;  criteria are met. Whether its shortest or not depends on the
;;;;  type of algorithm used and the type of heuristic. In general,
;;;;  the algorithm and heuristc will find the shortest one first
;;;;  because it typically looks for the solution in terms of the
;;;;  lowest cost first.
;;;; 
;;;; ----------------------------------------------------------------

(defun palindromep (state)
  "Does STATE represent a palindrome?"
  (vertex-stop? (state-vertex state)))

(defun pangramp (state)
  "Does STATE represent a pangram?"
  (= *goal-bvms* (state-bvms state)))

(defmethod goal-test ((problem spp-problem) state)
  "Does STATE represent both a palindrome and a pangram?"
  (and (pangramp state)
       (palindromep state)))

;;;; ----------------------------------------------------------------
;;;;
;;;;  Successors
;;;;
;;;;  Palindromes are built in this program by alternately adding
;;;;  words to either side such that the two sides are always mirrors
;;;;  of each other. Words are always added to the shorter side.
;;;;  Whichever characters are left on the longer side that do not
;;;;  have a match on the shorter side are used to find all the words
;;;;  that are legal to place on the other side.
;;;;
;;;;  The actions in the program that take us from one state to
;;;;  the next are the words that we add. The cost of each word
;;;;  added is simply the length of the word. The sum of the lengths
;;;;  of all the words added represent the total cost of the path.
;;;; 
;;;; ----------------------------------------------------------------

(defparameter *noisy* t
  "Controls whether certain messages are logged or not.")


(defmethod successors ((problem spp-problem) state)
  "Return a list of (action . state) pairs. An action is a pair of the
  form (side . word), where side represents the side to add the word to.
  The state is effectively a string representing the remaining letters."
  (incf (successor-calls problem))
  (incf (vertex-successor-calls (state-vertex state)))
  (loop for e in (vertex-out (state-vertex state))
	for v = (edge-to e)
	for bvms = (logior (state-bvms state) (edge-mask e))
	collect (cons e (make-state :vertex v :bvms bvms))))

(defun reset-branching-count ()
  (setf *successor-states* 0
	*successor-calls* 0))

(defun average-branching-factor ()
  (floor *successor-states* *successor-calls*))

;;;; ----------------------------------------------------------------
;;;;  h-cost (heuristic estimate of cost to attain goal)
;;;; ----------------------------------------------------------------

(defvar *sig-count* (make-hash-table))

(defvar *h-cost-count*)

(defun invert-sig (sig)
  (logxor sig *goal-bvms*))

(defvar *letter-cost-divisor* 50)

(defun set-letter-cost-divisor (n)
  (setf *letter-cost-divisor* n))

(defun letter-cost (c)
  (let ((p (1+ (position c "esiarntolcdupmghbyfvkwzxqj"))))
    (ceiling (/ (* p p) *letter-cost-divisor*))))

(defun simple-h-cost (bvms lhs rhs problem state)
  (if (goal-test problem state)
    0
    (let ((c (logcount bvms)))
      (+ (- (* (- 26 c) 2)
	    (if (= c 26) 0 1))
	 (length (or lhs rhs))))))

(defun ha-h-cost (bvms vertex)
  (loop for i from 0 to 25
	maximize (if (logbitp i bvms)
		   0
		   (aref (vertex-ha vertex) i))))

(defmethod h-cost ((problem spp-problem) state)
  "The estimated cost from state to a goal for this problem.  
  If you don't overestimate, then A* will always find optimal solutions.
  The default estimate is always 0, which certainly doesn't overestimate."
  (with-accessors ((bvms state-bvms) (vertex state-vertex)) state
    (with-accessors ((lhs vertex-lhs) (rhs vertex-rhs)) vertex
      (ecase (heuristic problem)
	(simple (simple-h-cost bvms lhs rhs problem state))
	(ha	(max (simple-h-cost bvms lhs rhs problem state)
		     (ha-h-cost bvms vertex)))
	(spcost (+ (length (or lhs rhs))
		   (max 0 (1- (* 2 (aref *costs* (invert-sig bvms)))))))
	(rarity (loop for c across *letters*
		      for p from 0
		      if (not (logbitp p bvms))
			sum (letter-cost c) into x
		      finally (return (* x 2))))))))
      
;;;; ----------------------------------------------------------------
;;;;  Edge Cost (actual cost added for a single edge)
;;;; ----------------------------------------------------------------

(defmethod edge-cost ((problem spp-problem) node action state)
  "The cost of going from one node to the next state by taking action.
  This default method counts 1 for every action.  Provide a method for this if 
  your subtype of problem has a different idea of the cost of a step."
  (edge-length action))

;;;; ----------------------------------------------------------------
;;;;  initialization
;;;; ----------------------------------------------------------------

(defun make-problem-ita (net &key (heuristic 'simple))
  (make-spp-problem
   :network net
   :initial-state (make-initial-state net)
   :heuristic heuristic))

(defun make-initial-state (net)
  (make-state :vertex (network-root net) :bvms 0))

(defstruct stats
  h-cost-count
  max-h-cost
  min-h-cost)

(defvar *problem*)

(defvar *words*)

(defvar *rwords*)

(defun init (net &key (heuristic 'simple))
;  (init-pangram-data)
  (setf *problem* (make-problem-ita net :heuristic heuristic)
	*sig-count* (make-hash-table)
	*h-cost-count* 0
	*successor-calls* 0
	*successor-states* 0)
  'ready)

(defun reinit ()
  (setf (evens-found *problem*) nil
	(odds-found *problem*) nil)
  *problem*)

(setf *print-right-margin* 1024)

(defmethod print-object ((state state) stream)
  (with-accessors ((bvms state-bvms) (vertex state-vertex)) state
    (with-accessors ((lhs vertex-lhs) (rhs vertex-rhs)) vertex
      (format stream "#<STATE :lhs ~a :rhs ~a :bvms ~26,'0B>" lhs rhs bvms))))

(defvar *solution*)

(defun run ()
  (setf *solution* (solve *problem*))
  'done)

(defun find-spp (net &key (algorithm 'simulated-annealing-search)
		 (heuristic 'simple) (use-worth-collecting-filter t)
		 (show-solution nil) (use-logging nil))
  "Given a list of words, return the shortest palindrome pangram (SPP)
  that is possible by forming a list of those words. Or, if no palindrome
  pangram is possible given the list, just return NIL."
  (init net :heuristic heuristic)
  (setf (use-worth-collecting-filter *problem*) use-worth-collecting-filter)
  (setf (use-logging *problem*) use-logging)
  (setf *solution* (solve *problem* algorithm show-solution))
  (if (and show-solution *solution*) (display-expand *problem* *solution* 0 0))
  (format-solution *solution*))

(defun format-solution (node)
  "Prints a nice version of the node to show the palindrome found
  as a simple string that is a nice list of words."
  (if node
      (loop for n = node then (aima::node-parent n)
	    for p = (aima::node-parent n) while p
	    for w = (edge-word (aima::node-action n))
	    for s = (aima::node-state p)
	    if (vertex-lhs (state-vertex s))
	      collect w into rhs else collect w into lhs
	    finally (return (let ((r (append (reverse lhs) rhs)))
			      (values (format nil "~{~a~^ ~}" r)
				      (loop for x in r sum (length x))))))))

; #("gbtnryx" "elakocqd" "pu" "shj" "fmwvziizv" "wmfj" "hsupdqc" "okal" "exyrntbg")
; #("abcdefg" "hijklmno" "pq" "rst" "uvwxyzzyx" "wvut" "srqponm" "lkji" "hgfedcba")
;    abcdefgh   ijkl mnopqrs     tuvw

(defun show-cache (cache)
  (loop for k being the hash-keys of cache using (hash-value v)
	for n = (length v)
	collect (cons k n) into pairs
	finally (loop for (k . n) in (sort pairs #'< :key #'cdr) do
		      (format t "~a: ~a~%" k n))))

(defmethod display-expand ((problem spp-problem) node count total)
  "Possibly print information when a node is expanded."
  (let ((state (node-state node)))
    (write-log "~2d ~3d ~3d ~3d ~6d ~9d ~3d ~26,'0B ~a ~10a ~10a~%"
	       (node-depth node)
	       (node-f-cost node)
	       (node-g-cost node)
	       (node-h-cost node)
	       count
	       total
	       (logcount (state-bvms state))
	       (state-bvms state)
	       (cond ((vertex-lhs (state-vertex state)) 'lhs)
		     ((vertex-rhs (state-vertex state)) 'rhs))
	       (or (vertex-lhs (state-vertex state))
		   (vertex-rhs (state-vertex state)))
	       (if (node-action node) (edge-word (node-action node))))))

(defun has-pangramp (words)
  "Does this set of words even have a pangram at all?"
  (loop with bvms = 0
	for word across words
	for sig = (word-bvms word)
	if (= bvms *goal-bvms*) do (return t)
	else do (setq bvms (logior bvms sig))))

