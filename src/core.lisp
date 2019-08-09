(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote lambda) args expr)))

(defun caar (x)
  (car (car x)))

(defun cadr (x)
  (car (cdr x)))

(defun cdar (x)
  (cdr (car x)))

(defun cddr (x)
  (cdr (cdr x)))

;; tail recursive map helper function
(defun map0 (fn in acc)
  (if (nil? in)
    acc
    (map0 fn
          (cdr in)
          (cons (fn (car in)) acc))))

(defun map (fn in)
  (reverse! (map0 fn in (quote ()))))

;; tail recursive filter helper function
(defun filter0 (pred in acc)
  (if (nil? in)
    acc
    (if (pred (car in))
      (filter0 pred
               (cdr in)
               (cons (car in) acc))
      (filter0 pred
               (cdr in)
               acc))))

(defun filter (pred in)
  (reverse! (filter0 pred in (quote ()))))

;; tail recursive reduce helper function
(defun reduce0 (fn in acc)
  (if (nil? in)
    acc
    (reduce0 fn
             (cdr in)
             (fn acc (car in)))))

(defun reduce (fn in)
  ; initial value of acc is (fn (car in) (cadr in))
  (reduce0 fn
           (cdr (cdr in))
           (fn (car in) (cadr in))))

(defun not (p)
  (nil? p))

(defun inc (x)
  (+ x 1))

(defun str-len0 (string acc)
  (if (nil? (char-at string acc))
    acc
    (str-len0 string (inc acc))))

(defun str-len (string)
  (str-len0 string 0))

(defun range0 (curr max acc)
  (if (< curr max)
    (range0 (inc curr) max (cons curr acc))
    acc))

(defun range1 (min max)
  (reverse! (range0 min max (quote ()))))

(defun range (max)
  (range1 0 max))

(defun to-chars (string)
  (map (lambda (x) (char-at string x))
       (range (str-len string))))

(defun rev-str (string)
  (reduce str-cat (reverse! (to-chars string))))

(defun max0 (list acc)
  (if (nil? list)
    acc
    (if (< acc (car list))
      (max0 (cdr list) (car list))
      (max0 (cdr list) acc))))

(defun max (list)
  (max0 (cdr list) (car list)))

(defun flatten (tree)
  (if (nil? tree)
    nil
    (if (list? tree)
      ;(map flatten tree)
      nil
      (list tree))))

(defun flatten (tree)
  (if (nil? tree)
    nil
    (if (list? tree)
      (reduce concat (map flatten tree))
      ; (map flatten2 tree)
      (list tree)
      )))

(defun concat0 (a b acc)
  (if (nil? a)
    (if (nil? b)
      acc
      (concat0 nil (cdr b) (cons (car b) acc)))
    (concat0 (cdr a) b (cons (car a) acc))))

(defun concat (a b)
  (reverse! (concat0 a b (quote ()))))

(defun append0 (in obj acc)
  (if in
    (append0 (cdr in) obj (cons (car in) acc))
    (cons obj acc)))

(defun append (in obj)
  (reverse! (append0 in obj (quote ()))))
