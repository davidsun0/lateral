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
  (reverse! (map0 fn in (quote ()))))

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
  (reverse! (filter0 pred list (quote ()))))

;; tail recursive reduce helper function
(defun reduce0 (fn list acc)
  (if (nil? list)
    acc
    (reduce0 fn
             (cdr list)
             (fn acc (car list)))))

(defun reduce (fn list)
  ; initial value of acc is (fn (car list) (cadr list))
  (reduce0 fn
           (cdr (cdr list))
           (fn (car list) (car (cdr list)))))

(defun not (p)
  (if p
    nil
    t))

(defun inc (x)
  (+ x 1))

(defun str-len0 (string acc)
  (if (nil? (char-at string acc))
    acc
    (str-len0 string (inc acc))))

(defun str-len (string)
  (str-len0 string 0))

(defun range0 (max curr acc)
  (if (< curr max)
    (range0 max (inc curr) (cons curr acc))
    acc))

(defun range1 (min max)
  (reverse! (range0 max min (quote ()))))

(defun range (max)
  (range1 0 max))

(print (range 10))
(print (reverse! (range 10)))

(defun to-chars (string)
  (map (lambda (x) (char-at string x))
       (range (str-len string))))

(print (to-chars "hello world!"))
(print (rev-str "hello world?"))
