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

;; tail recursive filter helper function
(defun filter0 (pred list acc)
  (if (nil? list)
    acc
    (if (pred (car list))
      (filter0 pred
               (cdr list)
               (cons (car list) acc))
      (filter0 pred
               (cdr list)
               acc))))

(defun filter (pred list)
  (reverse! (filter0 pred list ())))

(defun inc (x)
  (+ x 1))

(defun not (p)
  (if p
    nil
    t))

(defun not-nil? (p)
  (not (nil? p)))

(print (map inc (list 1 2 3)))
(print (map (lambda (x) (not (nil? x))) (list 1 nil 2 nil 3)))
(print (filter (lambda (x) (not (nil? x))) (list 1 nil 2 nil 3)))
(print (list 1 nil 2))
;(print (not t))
