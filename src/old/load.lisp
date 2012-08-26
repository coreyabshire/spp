; load the files

(defparameter *source-files*
  '("utils"
    "bsearch"
    "dictionary"
    "package"
    "constants"
    "words"
    "palindrome"
    "dfs-search"
    "bfs-solver"
    "pp"))

(defun load-all ()
  (every (lambda (filename) (load (compile-file filename))) *source-files*))
  