(defun identity (x)
  x)

(defun not (p)
  (if p nil t))

(defun length (in acc)
  (if in
    (length (rest in) (inc acc))
    acc))

(defun str-len (str n)
  (if (char-at str n)
    (str-len str (inc n))
    n))

(defun print-chars (str n)
  (if (char-at str n)
    (progn
      (print (char-at str n))
      (print-chars str (inc n)))
    nil))

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

(defun read0 (in-string tok-start idx acc)
  (if (char-at in-string idx)
    (cond
      (whitespace? (char-at in-string idx))
      (read0 in-string (inc idx) (inc idx)
             (make-token in-string tok-start idx acc))

      (equal? (char-at in-string idx) (char "("))
      (read0 in-string (inc idx) (inc idx)
             (cons (char "(") (make-token in-string tok-start idx acc)))
      
      (equal? (char-at in-string idx) (char ")"))
      (read0 in-string (inc idx) (inc idx)
             (cons (char ")") (make-token in-string tok-start idx acc)))
      
      t (read0 in-string tok-start (inc idx) acc)
      )
    (reverse acc)))

(defun read ()
  (progn
    (pprint "user> ")
    (read0 (readline) 0 0 nil)))

(defun main ()
  (progn
    (println (read))
    (main)))
