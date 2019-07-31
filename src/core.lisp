(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote lambda) args expr)))

;; tail recursive map helper function
(defun map0 (fn list acc)
  (if (nil? list)
    acc
    (map0 fn
          (cdr list)
          (cons (fn (car list)) acc))))

(defun map (fn in)
  (reverse! (map0 fn in ())))

(defun inc (x)
  (+ x 1))

(print (map inc (list 1 2 3)))
