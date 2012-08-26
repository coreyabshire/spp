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
;;;;  SPP-TEST - This one is used to encapsulate all the unit and
;;;;             functional tests that are written against the main
;;;;             SPP package.
;;;;
;;;;  SPP-USER - This is the one to get in if you want to run the
;;;;             program to find the SPP for a given word list.
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

(defstruct (state (:conc-name nil))
  "A state in this search space is a combination of any unmatched 
  characters for the current palindrome under construction, plus
  a BVMS (bit-vector member summary) representing the letters
  that have been matched thus far towards having a complete
  palindrome. A goal state is found when the unmatched characters
  are themselves a palindrome, and the bit-vector is all 1's.
  Each action is the word added to this state to derive a new
  state. Thus, the path to the goal state represents the set of
  words that comprise the ultimate response to the challenge."
  lhs
  rhs
  (bvms 0 :type fixnum))

(defstruct (palindrome-problem
	     (:include problem (initial-state (make-state)))
	     (:conc-name nil))
  "An overall definition of the problem of finding the shortest
  pangram palindrome. This structure is used to hold all the various
  word lists and caches, and also has the methods called by the
  AIMA search framework. (successors, goal-test, h-cost, etc...)"
  (heuristic 'simple)
  words
  (starting-cache (make-hash-table :test 'equal))
  (ending-cache (make-hash-table :test 'equal))
  evens-found
  odds-found
  letters
  (use-worth-collecting-filter nil)
  (use-logging nil))

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
  (let ((s (or (lhs state) (rhs state))))
    (and (string= s (reverse s)))))

(defun pangramp (state)
  "Does STATE represent a pangram?"
  (= *goal-bvms* (bvms state)))

(defmethod goal-test ((problem palindrome-problem) state)
  "Does STATE represent both a palindrome and a pangram?"
  (and (pangramp state)
       (palindromep state)))

(defmethod goal-test :after ((problem palindrome-problem) state)
  nil)

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

(defun apply-action (state action)
  (let ((lhs (or (lhs state) action))
	(rhs (or (rhs state) action)))
    (let ((ln (length lhs))
	  (rn (length rhs))
	  (bvms (logior (word-bvms action) (bvms state))))
      (cond ((= ln rn) (make-state :bvms bvms))
	    ((> ln rn) (make-state :bvms bvms :lhs (subseq lhs rn)))
	    ((< ln rn) (make-state :bvms bvms :rhs (subseq rhs 0 (- rn ln))))))))

(defparameter *noisy* t
  "Controls whether certain messages are logged or not.")

;
; This has a bug as defined below. The problem is that it misses words
; that could potentially complete the palindrome.
; TODO: more on this...
;
; What I had been trying to say with this procedure is this:
;
;   If the target of the next state is not itself a palindrome,
;   or if there are no successor states for that state, then it
;   is not even worth collecting.
;
; However, for some reason it failed for this test:
;
;     abc defghijk lmnop qrstu vwxy zyxwvutsrqp onmlkji hgfedcba
;
; When it tried to find words ending in p, it did not find zyxwvutsrap.
; This is because zyxwvutsrapq is not a palindrome, and there is no words
; starting with the inverse.
;
; It turns out the bug was with has-word-starting. I had only half finished
; that one when I was working on has-word-ending. It was using the wrong
; sort function for the compare in the smaller equality search and using
; smaller-ending where it should have been using smaller-starting.

(defun worth-collecting-p (problem state)
  "Determine whether or not the given state is worth collecting."
  (with-accessors ((lhs lhs) (rhs rhs) (bvms bvms)) state
    (or (goal-test problem state)
	(and lhs (has-word-ending (reverse lhs) (words problem)))
	(and rhs (has-word-starting (reverse rhs) (words problem)))
	(and (null lhs) (null rhs)))))

(defun actions-starting (problem rhs)
  "Computes all thenext actions for the problem given the right hand
  side RHS from the last word added."
  (multiple-value-bind (value hit) (gethash rhs (starting-cache problem))
    (if hit
	value
	(setf (gethash rhs (starting-cache problem))
	      (let ((initial (make-state :rhs rhs :bvms 0))
		    (result nil))
		(do-words-starting (action (reverse rhs) (words problem) result)
		  (let ((state (apply-action initial action)))
		    (if (or (not (use-worth-collecting-filter problem))
			    (worth-collecting-p problem state))
			(push (cons action state) result)))))))))

(defun actions-ending (problem lhs)
  "Computes all the next actions for the problem given the left hand
  side LHS from the last word added."
  (multiple-value-bind (value hit) (gethash lhs (ending-cache problem))
    (if hit
	value
	(setf (gethash lhs (ending-cache problem))
	      (let ((initial (make-state :lhs lhs :bvms 0))
		    (result nil))
		(do-words-ending (action (reverse lhs) (words problem) result)
		  (let ((state (apply-action initial action)))
		    (if (or (not (use-worth-collecting-filter problem))
			    (worth-collecting-p problem state))
			(push (cons action state) result)))))))))

(defun combine-states (state delta)
  "Given a delta as returned from actions-ending or actions-starting,
  combines it with the current state to form a new state."
  (make-state :lhs (lhs delta)
	      :rhs (rhs delta)
	      :bvms (logior (bvms delta)
			    (bvms state))))

(defmethod successors ((problem palindrome-problem) state)
  "Return a list of (action . state) pairs. An action is a pair of the
  form (side . word), where side represents the side to add the word to.
  The state is effectively a string representing the remaining letters."
  (with-accessors ((lhs lhs) (rhs rhs)) state
    (let ((s ()))
      (cond (lhs (loop for (action . delta) in (actions-ending problem lhs)
		    do (push (cons action (combine-states state delta)) s)))
            (rhs (loop for (action . delta) in (actions-starting problem rhs)
		    do (push (cons action (combine-states state delta)) s)))
            (t   (loop for (action . delta) in (actions-starting problem "")
		    do (push (cons action (combine-states state delta)) s))))
      s)))

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

(defmethod h-cost ((problem palindrome-problem) state)
  "The estimated cost from state to a goal for this problem.  
  If you don't overestimate, then A* will always find optimal solutions.
  The default estimate is always 0, which certainly doesn't overestimate."
  (with-accessors ((lhs lhs) (rhs rhs) (bvms bvms)) state
    (ecase (heuristic problem)
      (simple (if (goal-test problem state)
		  0
		  (let ((c (logcount (bvms state))))
		    (+ (- (* (- 26 c) 2)
			  (if (= c 26) 0 1))
		       (length (or lhs rhs))))))
      (spcost (+ (length (or lhs rhs))
		 (max 0 (1- (* 2 (aref *costs* (invert-sig bvms)))))))
      (rarity (loop for c across *letters*
		    for p from 0
		    if (not (logbitp p (bvms state)))
		    sum (letter-cost c) into x
		    finally (return (* x 2)))))))
      
;;;; ----------------------------------------------------------------
;;;;  Edge Cost (actual cost added for a single edge)
;;;; ----------------------------------------------------------------

(defmethod edge-cost ((problem palindrome-problem) node action state)
  "The cost of going from one node to the next state by taking action.
  This default method counts 1 for every action.  Provide a method for this if 
  your subtype of problem has a different idea of the cost of a step."
  (length action))

;;;; ----------------------------------------------------------------
;;;;  initialization
;;;; ----------------------------------------------------------------

(defun make-problem-ita (wordlist &key (heuristic 'simple))
  (make-palindrome-problem
   :words (make-words-from-list wordlist)
   :heuristic heuristic))

(defstruct stats
  h-cost-count
  max-h-cost
  min-h-cost)

(defvar *problem*)

(defvar *words*)

(defvar *rwords*)

(defun init (wordlist &key (heuristic 'simple))
;  (init-pangram-data)
  (setf *problem* (make-problem-ita wordlist :heuristic heuristic)
	*words* (words *problem*)
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
  (with-accessors ((lhs lhs) (rhs rhs) (bvms bvms)) state
    (format stream "#<STATE :lhs ~a :rhs ~a :bvms ~26,'0B>" lhs rhs bvms)))

(defvar *solution*)

(defun run ()
  (setf *solution* (solve *problem*))
  'done)

(defun find-spp (&key (algorithm 'aima::a*-search) (basename nil) (wordlist nil)
		 (heuristic 'simple) (use-worth-collecting-filter t)
		 (show-solution nil) (use-logging nil))
  "Given a list of words, return the shortest palindrome pangram (SPP)
  that is possible by forming a list of those words. Or, if no palindrome
  pangram is possible given the list, just return NIL."
  (cond (basename (init (read-words (data-file basename)) :heuristic heuristic))
	(wordlist (init wordlist :heuristic heuristic)))
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
	    for w = (aima::node-action n)
	    for s = (aima::node-state p)
	    if (lhs s) collect w into rhs else collect w into lhs
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

(defmethod display-expand ((problem palindrome-problem) node count total)
  "Possibly print information when a node is expanded."
  (let ((state (node-state node)))
    (write-log "~2d ~3d ~3d ~3d ~6d ~9d ~26,'0B ~a ~10a ~10a~%"
	       (node-depth node)
	       (node-f-cost node)
	       (node-g-cost node)
	       (node-h-cost node)
	       count
	       total
	       (bvms state)
	       (cond ((lhs state) 'lhs)
		     ((rhs state) 'rhs))
	       (or (lhs state) (rhs state))
	       (node-action node))))

(defun has-pangramp (words)
  "Does this set of words even have a pangram at all?"
  (loop with bvms = 0
	for word across words
	for sig = (word-bvms word)
	if (= bvms *goal-bvms*) do (return t)
	else do (setq bvms (logior bvms sig))))
