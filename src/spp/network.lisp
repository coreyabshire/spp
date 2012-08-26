;;;; -------------------------------------------------------------------------
;;;; Generalize a network based on the set of palindromes.
;;;; Kind of like substate-walk, but more generic, and able to 
;;;; be used as a basis for a fresh try at A*search.
;;;; Also should be easier to analyze, like the substate ideas,
;;;; as a graph.
;;;; -------------------------------------------------------------------------


(in-package :spp)

(defvar *temp-v-out*)
(defvar *net*)

;;;; -------------------------------------------------------------------------
;;;; network
;;;; -------------------------------------------------------------------------
;;;; Represents the entire palindrome network. A palindrome network consists
;;;; of a set of vertexes which represents the in process palindrome built
;;;; by adding words on either side of a palindrome. The edges are the words
;;;; that are added to either side.
;;;; -------------------------------------------------------------------------

(defstruct network
  words
  last-id
  root
  lhs-lookup
  rhs-lookup
  vertex-list
  edge-list
  word-edges)


;;;; -------------------------------------------------------------------------
;;;; vertex
;;;; -------------------------------------------------------------------------
;;;; A vertex in a palindrome network represents a point in the construction
;;;; process. Its effectively the letters left over after adding a word to one
;;;; side, that is legal to add based on the letters on the other side.
;;;; -------------------------------------------------------------------------

(defstruct vertex
  num
  out
  in
  stop?
  dead?
  expanded?
  lhs
  rhs
  (successor-calls 0)
  h0
  (ha (make-array 26))
  )

;;;; -------------------------------------------------------------------------
;;;; edge
;;;; -------------------------------------------------------------------------
;;;; Represents a word, that in constructing a palindrome, advances the 
;;;; construction by one step, from one vertex to the next. The word matches
;;;; the remainder on the appropriate side of the from vertex, and the piece
;;;; left over from the word forms the remainder represented by the to vertex.
;;;; -------------------------------------------------------------------------

(defstruct edge
  from
  to
  length
  mask
  word)



(defun string-pal? (s)
  (string= s (reverse s)))


;;;; -------------------------------------------------------------------------
;;;; lhs-vertex
;;;; -------------------------------------------------------------------------
;;;; Give it a left hand side remnant of what's not been matched in the 
;;;; palindrome in progress, and it will look it up to see if it has already
;;;; been created as a vertex in the network. If it has, it simply returns
;;;; the one that has already been created. If it has not, the it will create
;;;; the appropriate vertex object, register it in the lookup table, and
;;;; return that it instead.
;;;; -------------------------------------------------------------------------

(defun lhs-vertex (net lhs)
  (multiple-value-bind (val hit) (gethash lhs (network-lhs-lookup net))
    (if hit
	val
	(let ((vertex (make-vertex :num (incf (network-last-id net))
				   :lhs lhs
				   :stop? (string-pal? lhs))))
	  (setf (gethash lhs (network-lhs-lookup net)) vertex)
	  (push vertex (network-vertex-list net))
	  vertex))))




;;;; -------------------------------------------------------------------------
;;;; rhs-vertex
;;;; -------------------------------------------------------------------------
;;;; Give it a right hand side remnant of what's not been matched in the 
;;;; palindrome in progress, and it will look it up to see if it has already
;;;; been created as a vertex in the network. If it has, it simply returns
;;;; the one that has already been created. If it has not, the it will create
;;;; the appropriate vertex object, register it in the lookup table, and
;;;; return that it instead.
;;;; -------------------------------------------------------------------------

(defun rhs-vertex (net rhs)
  (multiple-value-bind (val hit) (gethash rhs (network-rhs-lookup net))
    (if hit
	val
	(let ((vertex (make-vertex :num (incf (network-last-id net))
				   :rhs rhs
				   :stop? (string-pal? rhs))))
	  (setf (gethash rhs (network-rhs-lookup net)) vertex)
	  (push vertex (network-vertex-list net))
	  vertex))))



(defun leftovers (lhs rhs word)
  (if (and (null lhs) (null rhs))
      (values word nil)
      (let ((lhs (or lhs word))
	    (rhs (or rhs word)))
	(let ((ln (length lhs))
	      (rn (length rhs)))
	  (cond ((= ln rn) (values nil nil))
		((> ln rn) (values (subseq lhs rn) nil))
		((< ln rn) (values nil (subseq rhs 0 (- rn ln)))))))))
    

;;;; -------------------------------------------------------------------------
;;;; edge-to-vertex-for-word
;;;; -------------------------------------------------------------------------
;;;; Finds or creates a vertex based on the remnant remaining after applying
;;;; word to the in progress palindrome represented by the FROM vertex. Returns
;;;; the edge, which contains WORD, the given FROM vertex, and the link to the
;;;; appropriate TO vertex, in the given NETwork.
;;;; -------------------------------------------------------------------------

(defun build-edge (net from word)
  (multiple-value-bind (lhs rhs)
      (leftovers (vertex-lhs from) (vertex-rhs from) word)
    (let ((to (cond (lhs (lhs-vertex net lhs))
		    (rhs (rhs-vertex net rhs))
		    (t (network-root net)))))
      (assert (not (equalp from to)))
      (let ((edge (make-edge :from from :to to :length (length word)
			     :mask (word-bvms word) :word word)))
	(multiple-value-bind (val hit) (gethash word (network-word-edges net))
	  (declare (ignore val))
	  (unless hit (setf (gethash word (network-word-edges net)) ()))
	  (push edge (gethash word (network-word-edges net))))
	(push edge (network-edge-list net))
	edge))))

(defmethod print-object ((vertex vertex) stream)
  (format stream "#<VERTEX :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a>"
	  :num   (vertex-num vertex)
	  :lhs   (vertex-lhs vertex)
	  :rhs   (vertex-rhs vertex)
	  :out   (length (vertex-out vertex))
	  :in    (length (vertex-in vertex))
	  :stop? (vertex-stop? vertex)
	  :dead? (vertex-dead? vertex)
	  :calls (vertex-successor-calls vertex)
	  :h0    (vertex-h0 vertex)
	  :ha    (vertex-ha vertex)))


(defmethod print-object ((network network) stream)
  (format stream "#<NETWORK :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a :~a ~a>"
	  :words       (length (words-forward (network-words network)))
	  :last-id     (network-last-id network)
	  :root        (not (null (network-root network)))
	  :lhs-lookup  (hash-table-count (network-lhs-lookup network))
	  :rhs-lookup  (hash-table-count (network-rhs-lookup network))
	  :vertex-list (length (network-vertex-list network))
	  :edge-list   (length (network-edge-list network))))


(defmethod print-object ((edge edge) stream)
  (format stream "#<EDGE :~a ~26,'0B :~a ~3d :~a ~a :~a ~a (~a;~a) :~a ~a (~a;~a)>"
	  :mask    (edge-mask edge)
	  :length  (edge-length edge)
	  :using   (edge-word edge)
	  :from    (vertex-num (edge-from edge))
	           (vertex-lhs (edge-from edge))
		   (vertex-rhs (edge-from edge))
	  :to      (vertex-num (edge-to edge))
	           (vertex-lhs (edge-to edge))
	           (vertex-rhs (edge-to edge))))


(defun edges-from-lhs (net from lhs)
  (let ((edges ()))
    (do-words-ending (word (reverse lhs) (network-words net) edges)
      (push (build-edge net from word) edges))))




(defun edges-from-rhs (net from rhs)
  (let ((edges ()))
    (do-words-starting (word (reverse rhs) (network-words net) edges)
      (push (build-edge net from word) edges))))




(defun edges-from-root (net from)
  (loop for word across (words-forward (network-words net))
	collect (build-edge net from word)))




(defun edges-from-vertex (net from)
  (with-accessors ((lhs vertex-lhs) (rhs vertex-rhs)) from
    (cond (lhs (edges-from-lhs net from lhs))
	  (rhs (edges-from-rhs net from rhs))
	  (t (edges-from-root net from)))))




(defun resolve-in-from-out (edges)
  (loop for edge in edges do
    (with-accessors ((from edge-from) (to edge-to)) edge
      (push from (vertex-in to)))))
	



;;;; -------------------------------------------------------------------------
;;;; expand-vertex
;;;; -------------------------------------------------------------------------
;;;; Recursively expands the palindrome network from this vertex on.
;;;; -------------------------------------------------------------------------

(defun expand-vertex (net from)
  (unless (vertex-expanded? from)
    (let ((edges (edges-from-vertex net from)))
      (setf (vertex-out from) edges)
      (setf (vertex-expanded? from) t)
      (resolve-in-from-out edges)
      (loop for edge in edges do
	(expand-vertex net (edge-to edge))))))


(defun make-network-empty (words)
  (let ((root (make-vertex :num 1 :stop? t)))
    (make-network :words words
		  :last-id 1
		  :root root
		  :lhs-lookup (make-hash-table :test 'equal)
		  :rhs-lookup (make-hash-table :test 'equal)
		  :vertex-list (list root)
		  :edge-list nil
		  :word-edges (make-hash-table :test 'equal)
		  )))


(defun make-network-from-words (words)
  (let ((net (make-network-empty words)))
    (expand-vertex net (network-root net))
    (mark-deadends net)
    (loop for c = (mark-more-deadends net) while (> c 0))
    net))

(defun make-filtered-network (unet)
  (let ((net (make-network-empty (network-words unet)))
	(vuhash (make-hash-table :test 'eq))
	(uvhash (make-hash-table :test 'eq))
	(vlist (reverse (remove-if #'vertex-dead? (network-vertex-list unet)))))
    (setf (gethash (network-root unet) uvhash) (network-root net))
    (setf (gethash (network-root net) vuhash) (network-root unet))
    (loop for u in vlist for lhs = (vertex-lhs u) for rhs = (vertex-rhs u)
	  if (or lhs rhs)
	    do (let ((v (make-vertex :num (incf (network-last-id net))
				     :out nil :in nil :stop? (vertex-stop? u)
				     :dead? (vertex-dead? u)
				     :expanded? nil :lhs lhs :rhs rhs)))
		 (setf (gethash v vuhash) u)
		 (setf (gethash u uvhash) v)
		 (push v (network-vertex-list net))
		 (if lhs (setf (gethash lhs (network-lhs-lookup net)) v))
		 (if rhs (setf (gethash rhs (network-rhs-lookup net)) v))))
    (loop for v in (network-vertex-list net)
	  for u = (gethash v vuhash)
	  do (loop for d in (vertex-out u)
		   for to = (gethash (edge-to d) uvhash)
		   for e = (make-edge :from v :to to :length (edge-length d)
				:mask (edge-mask d) :word (edge-word d)) 
		   unless (vertex-dead? (edge-to d))
		     do (push e (vertex-out v))
			(push e (network-edge-list net))))
    (loop for v in (network-vertex-list net)
	  do (resolve-in-from-out (vertex-out v)))
    (loop for e in (network-edge-list net) for w = (edge-word e)
	  do (multiple-value-bind (val hit) (gethash w (network-word-edges net))
	       (declare (ignore val))
	       (unless hit (setf (gethash w (network-word-edges net)) ()))
	       (push e (gethash w (network-word-edges net)))))
    net))
			       

(defun mark-deadends (net)
  (loop for vertex in (network-vertex-list net)
	if (and (= 0 (length (vertex-out vertex)))
		(not (vertex-stop? vertex)))
	  do (setf (vertex-dead? vertex) t)))


(defun extended-dead? (vertex)
  (and (not (vertex-stop? vertex))
       (vertex-dead? vertex)))


(defun mark-more-deadends (net)
  (loop with c = 0
	for v in (network-vertex-list net)
	if (and (every #'extended-dead? (mapcar #'edge-to (vertex-out v)))
		(not (vertex-stop? v)))
	  do (progn (when (not (vertex-dead? v))
		      (setf (vertex-dead? v) t)
		      (incf c)))
	finally (return c)))



(defun reset-successor-calls (net)
  (dolist (v (network-vertex-list net))
    (setf (vertex-successor-calls v) 0)))


(defun write-keys-and-counts (stream hash-table)
  (loop for k being the hash-keys of hash-table
	using (hash-value v)
	do (format stream "~20a ~10d~%" k (length v))))

(defun write-network-word-edge-counts (net filespec)
  (with-open-file (out filespec
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (write-keys-and-counts out (network-word-edges net))))

(defun network-edge-words (net)
  (loop for k being the hash-keys of (network-word-edges net)
	collect k))

(defun write-word-file (basename words)
  (with-open-file (out (data-file basename)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (dolist (word words)
      (format out "~a~%" word))))

(defun letter-summary (stream words)
  (let ((hash-table (make-hash-table)))
    (loop for c across *letters* do
      (setf (gethash c hash-table) 0)
      (loop for w in words do
	(if (find c w)
	  (incf (gethash c hash-table)))))
    (loop for k being the hash-keys of hash-table
	  using (hash-value v)
	  do (format stream "~a ~a~%" k v))
    hash-table))

(defun init-h0 (net)
  (loop for v in (network-vertex-list net)
	count (setf (vertex-h0 v)
		    (if (vertex-stop? v)
		      0
		      nil))))

(defun step-h0 (net)
  (loop for e in (network-edge-list net)
	count (if (vertex-h0 (edge-to e))
		(let ((new-h0 (+ (vertex-h0 (edge-to e)) (edge-length e)))
		      (old-h0 (vertex-h0 (edge-from e))))
		  (if (or (null old-h0) (< new-h0 old-h0))
		    (setf (vertex-h0 (edge-from e)) new-h0))))))

(defun init-ha (net c)
  (loop with i = (position c *letters*)
	for e in (network-edge-list net)
	count (setf (aref (vertex-ha (edge-from e)) i)
		    (if (and (vertex-stop? (edge-from e))
			     (find c (or (vertex-lhs (edge-from e))
					 (vertex-rhs (edge-from e)))))
		      0
		      (if (find c (edge-word e))
			(+ (vertex-h0 (edge-to e)) (edge-length e))
			nil)))))

(defun step-ha (net c)
  (loop for e in (network-edge-list net)
	for i = (position c *letters*)
	count (if (aref (vertex-ha (edge-to e)) i)
		(let ((new-hq (+ (aref (vertex-ha (edge-to e)) i) (edge-length e)))
		      (old-hq (aref (vertex-ha (edge-from e)) i)))
		  (if (or (null old-hq) (< new-hq old-hq))
		    (setf (aref (vertex-ha (edge-from e)) i) new-hq))))))

(defun init-vertex-h (net)
  (init-h0 net)
  (loop while (> (step-h0 net) 0))
  (loop for v in (network-vertex-list net)
	if (null (vertex-h0 v))
	  do (setf (vertex-h0 v) 255))
  (loop for c across *letters*
	do (init-ha net c)
	   (loop while (> (step-ha net c) 0))))

;(defvar *hqj* (make-array (length (network-vertex-list *net*))))

(defun init-hqj (net)
  (loop for e in (network-edge-list net)
	for ef = (edge-from e)
	for et = (edge-to e)
	for i = (- (vertex-num ef) 1)
	do (setf (aref *hqj* i) i)))
	

(defun load-network (basename)
  (let ((net (make-filtered-network
	      (make-network-from-words
	       (make-words-from-file
		(data-file basename))))))
    (init-vertex-h net)
    net))

(defun find-vertex (net lhs rhs)
  (cond (lhs (gethash lhs (network-lhs-lookup net)))
    (rhs (gethash rhs (network-rhs-lookup net)))
    (t (network-root net))))

(defun show-v-tree (out vertex depth limit &optional edge)
  (when (< depth limit)
    (format out "~a~a: ~a~%"
	    (make-string (* depth 4) :initial-element #\Space)
	    (if edge (edge-word edge) "")
	    vertex)
    (loop for e in (vertex-out vertex) do
      (show-v-tree out (edge-to e) (+ 1 depth) limit e))))

(defun quick-show (net lhs rhs depth limit)
  (with-open-file (out "/home/corey/Projects/SPP/logs/quick-show.txt"
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
    (show-v-tree out (find-vertex net lhs rhs) depth limit)))

(defun show-hc-path (v c &optional hit)
  (cond ((and (or hit (find c (or (vertex-lhs v) (vertex-rhs v)))) (vertex-stop? v)) ())
    (t (let ((i (position c *letters*)))
	 (let ((v-out (sort (copy-list (vertex-out v)) #'<
			    :key (lambda (e) (let ((v (edge-to e)) (n (edge-length e)))
				   (cond
				     ((and (vertex-stop? v) (find c (or (vertex-lhs v) (vertex-rhs v)))) (edge-length e))
				     (hit (+ n (vertex-h0 (edge-to e))))
				     (t (+ n (aref (vertex-ha (edge-to e)) i)))))))))
	   (setf *temp-v-out* v-out)
	   (let ((e (car v-out)))
	     (format t "~a (~a): ~a~%" (edge-word e) hit (edge-to e))
	     (cons (edge-word e) (show-hc-path (edge-to e) c (find c (edge-word e))))))))))

(defun prove-hc (net lhs rhs c)
  "Check whether there is really a path in the network for the vertex represented by 
  lhs rhs to a palindrome state that passes through an edge containing c, possible
  within the cost predicted by ha[c] that was precalculated for that vertex."
  (let ((v (find-vertex net lhs rhs)))
    (let ((pred-ha (aref (vertex-ha v) (position c *letters*))))
      (format t "from: ~a~%" v)
      (format t "pred-ha(~a): ~a~%" c pred-ha)
      (let ((path (show-hc-path v c)))
	(let ((real-ha (reduce #'+ path :key #'length)))
	  (format t "real-ha(~a): ~a (match? ~a)~%" c real-ha (= pred-ha real-ha))
	  (format t "~a~%" path))))))

(defun show-paths-to-root (vertex root &key (depth 1) (limit 2) (path ()))
  (cond
    ((and (> depth 1) (eq vertex root))
     (format t "~a~%" path)
     (finish-output))
    ((> depth limit) nil)
    (t (loop for e in (vertex-out vertex) do
      (show-paths-to-root (edge-to e) root
	   :depth (+ 1 depth)
	   :limit limit
	   :path (cons (edge-word e) path))))))


(defun count-paths-to-root (stream vertex root &key (depth 0) (target 2))
  (let ((n 0))
    (labels
	((paths-to-root (stream vertex root &key (depth 0) (target 2) (path ()))
	   (cond
	     ((and (= depth target) (eq vertex root))
	      (incf n)
	      (format stream "~a~%" (format-path-as-palindrome path)))
	     ((and (> depth 0) (eq vertex root))
	      nil)
	     ((> depth target)
	      nil)
	     (t
	      (loop for e in (vertex-out vertex) do
	       (paths-to-root stream (edge-to e) root
		    :depth (+ 1 depth)
		    :target target
		    :path (cons (edge-word e) path)))))))
      (paths-to-root stream vertex root :depth depth :target target)
      n)))

(defun run-count-paths-to-root (net &key (target 2))
  (with-open-file (out (format nil "/home/corey/Projects/SPP/logs/even-paths-to-root-depth-~a.txt" target)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (count-paths-to-root out (network-root net) (network-root net) :target target)))

(defun format-path-as-palindrome (path)
  (loop with lhs = ()
	with rhs = ()
	with ln = 0
	with rn = 0
	for w in (reverse path)
	for n = (length w) do
	  (if (> ln rn)
	    (setf rhs (cons w rhs)
		  rn (+ rn n))
	    (setf lhs (cons w lhs)
		  ln (+ ln n)))
	finally (return (format nil "~{~a~^ ~} ~{~a~^ ~}" (reverse lhs) rhs))))
	  
(defun dump-vertex-h-data (net name)
  (with-open-file (out (format nil "~a/logs/~a.txt" *basepath* name)
		       :direction :output :if-exists :supersede)
    (format out "h0 ~{h~a~^ ~}~%" (map 'list #'identity *letters*))
    (loop for v in (network-vertex-list net)
	  for h0 = (vertex-h0 v)
	  for ha = (map 'list #'identity (vertex-ha v))
	  do (format out "~a ~{~a~^ ~}~%" h0 ha))))

(defun full-h-cost (v bvms)
  (with-accessors ((lhs vertex-lhs) (rhs vertex-rhs) (ha vertex-ha) (h0 vertex-h0)) v
    (max
     (let ((c (logcount bvms)))
       (+ (- (* (- 26 c) 2)
	     (if (= c 26) 0 1))
	  (length (or lhs rhs))))
     (loop for i from 0 to 25
	   maximize (if (logbitp i bvms)
		      0
		      (aref ha i))))))


(defun tree-search (out v bvms path g limit h f)
  (format out "~26,'0B ~2d ~a ~a ~3d = ~3d + ~3d ~a ~%"
	  bvms (logcount bvms)
	  (if (vertex-stop? v) "Y" "N")
	  (if (= bvms *goal-bvms*) "Y" "N")
	  f g h
	  (format-path-as-palindrome (mapcar #'edge-word path)))
  (if (and (= bvms *goal-bvms*) (vertex-stop? v))
    path
    (loop for e in (vertex-out v)
	  for et = (edge-to e)
	  for ebvms = (logior bvms (edge-mask e))
	  for eh = (full-h-cost et ebvms)
	  for eg = (+ (edge-length e) g)
	  for ef = (+ eh eg)
	  ;do (format t "~26,'0B ~a ~a ~a~%" ebvms eh eg ef)
	  if (<= ef limit) do
	    (let ((r (tree-search out et ebvms (cons e path) eg limit eh ef)))
	      (if r (return r))))))

(defun run-tree-search (net &key (limit 51) (name "tree-search"))
  (with-open-log-file (out name)
    (let ((path (tree-search out (network-root net) 0 () 0 limit limit limit)))
      (if path
	(format-path-as-palindrome (mapcar #'edge-word path))))))

(defparameter *vowels*
  "aeiouy")

(defparameter *consonants*
  (remove-if (lambda (c) (find c *vowels*)) *letters*))

(defun letter-sets (s)
  "Create a string consisting of three words, where each
  word is a set of characters describing three distinct
  sets of characters based on the given string, which is
  taken to represent a palindrome. The three words are:
  1) The vowels that were seen in the palindrome.
  2) The consonants that were seen in the palindrome.
  3) The letters that are remaining to be found."
  (let (remaining consonants vowels)
    (loop for c across *letters* do
      (if (find c s)
	(if (find c *vowels*)
	  (push c vowels)
	  (push c consonants))
	(push c remaining)))
    (format nil "~{~a~} ~{~a~} ~{~a~}"
	    (reverse vowels)
	    (reverse consonants)
	    (reverse remaining))))

(defun log-file-name (name)
  (format nil "~a/logs/~a.txt" *basepath* name))

(defmacro with-open-log-file ((stream name) &body body)
  `(with-open-file (,stream (log-file-name ,name) :direction :output :if-exists :supersede)
     ,@body))
     

(defun write-level-one-costs (net name)
  (with-open-log-file (out name)
    (loop with bvms = (word-bvms "")
	  for e in (vertex-out (network-root net))
	  for v = (edge-to e)
	  for w = (edge-word e)
	  for h = (full-h-cost v (logior bvms (edge-mask e)))
	  for g = (edge-length e)
	  for f = (+ g h)
	  collect (list f g h w) into seq
	  finally (loop for s in (sort seq #'< :key #'car)
			do (format out "~{~a~^ ~}~%" s)))))

(defun write-edge-mask-uniqueness (net name)
  (loop with h = (make-hash-table)
	for e in (network-edge-list net)
	for m = (edge-mask e)
	do (multiple-value-bind (val hit) (gethash m h)
	     (setf (gethash m h) (+ (if hit val 0) 1)))
	finally (with-open-log-file (out name)
		  (loop for k being the hash-keys of h using (hash-value v)
			do (format out "~8d ~a ~3d ~8d~%"
				   k (reverse-bvms-expanded k) (logcount k) v)))))

(defun reverse-bvms (bvms)
  (loop for i from 0 below (length *letters*)
	if (logbitp i bvms)
	  collect (aref *letters* i) into s
	finally (return (map 'string #'identity s))))

(defun reverse-bvms-expanded (bvms)
  (loop for i from 0 below (length *letters*)
	collect (if (logbitp i bvms) (aref *letters* i) #\-) into s
	finally (return (map 'string #'identity s))))

(defun unique-vowel-count (s &optional (vowels *vowels*))
  (loop for v across vowels
	if (find v s) count v))

(defun write-edge-word-unique-vowel-count (net &optional (name "edge-unique-vowel-count"))
  (with-open-log-file (log name)
    (loop for w in (network-edge-words net)
	  do (format log "~2d ~a~%" (unique-vowel-count w) w))))

(defstruct ant
  vertex
  path
  distance
  bvms)

(defvar *ants*)

(defun init-ants ())
