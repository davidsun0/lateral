(defun second (list)
  (first (rest list)))

(defun reverse0 (in acc)
  (if in
    (reverse0 (rest in) (cons (first in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

(defun make-token (in-string start end acc)
  (if (equal? start end)
    acc
    (cons (substr in-string start end) acc)))

(defun tokens (in-string tok-start idx acc)
  (if (char-at in-string idx)
    (cond
      (whitespace? (char-at in-string idx))
      (tokens in-string (inc idx) (inc idx)
             (make-token in-string tok-start idx acc))

      (equal? (char-at in-string idx) "(")
      (tokens in-string (inc idx) (inc idx)
             (cons "(" (make-token in-string tok-start idx acc)))
      
      (equal? (char-at in-string idx) "(")
      (tokens in-string (inc idx) (inc idx)
             (cons "(" (make-token in-string tok-start idx acc)))
      
      t (tokens in-string tok-start (inc idx) acc)
      )
    (reverse acc)))

(defun read ()
  (progn
    (pprint "user> ")
    (tokens (readline) 0 0 nil)
))

(defun main ()
  (progn
    (println (read))
    (main)))
