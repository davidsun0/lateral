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
  (if in
    (map0 fn
          (cdr in)
          (cons (fn (car in)) acc))
    acc))

(defun map (fn in)
  (reverse! (map0 fn in nil)))

;; tail recursive filter helper function
(defun filter0 (pred in acc)
  (if in
    (if (pred (car in))
      (filter0 pred
               (cdr in)
               (cons (car in) acc))
      (filter0 pred
               (cdr in)
               acc))
    acc))

(defun filter (pred in)
  (reverse! (filter0 pred in nil)))

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

(defun inc (n)
  (+ n 1))

(defun dec (n)
  (- n 1))

(defun length0 (in acc)
  (if in
    (length0 (cdr in) (inc acc))
    acc))

(defun length (in)
  (length0 in 0))

(defun str-len0 (str acc)
  (if (nil? (char-at str acc))
    acc
    (str-len0 str (inc acc))))

(defun str-len (str)
  (str-len0 str 0))

(defun range0 (curr max acc)
  (if (< curr max)
    (range0 (inc curr) max (cons curr acc))
    acc))

(defun range1 (min max)
  (reverse! (range0 min max nil)))

(defun range (max)
  (range1 0 max))

(defun to-chars (str)
  (map (lambda (x) (char-at str x))
       (range (str-len str))))

(defun rev-str (str)
  (reduce str-cat (reverse! (to-chars str))))

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
  (reverse! (concat0 a b nil)))

(defun append0 (in obj acc)
  (if in
    (append0 (cdr in) obj (cons (car in) acc))
    (cons obj acc)))

(defun append (in obj)
  (reverse! (append0 in obj nil)))

(defun nth (n in)
  (if (= n 0)
    (car in)
    (nth (dec n) (cdr in))))

(defun last (in)
  (if (cdr in)
    (last (cdr in))
    (car in)))

(defun reverse0 (in acc)
  (if in
    (reverse0 (cdr in) (cons (car in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

(defun equal? (a b)
  (cond (and (nil? a) (nil? b)) t,
        (or (nil? a) (nil? b)) nil,
        (not (equal?0 (type a) (type b))) nil,
        (and (equal?0 (type a) :list) (equal?0 (car a) (car b)))
          (equal? (cdr a) (cdr b)),
        t (equal?0 a b)))

(defun list? (a)
  (equal? :list (type a)))

(defun int? (a)
  (equal? :int (type a)))

(defun keyword? (a)
  (equal? :keyword (type a)))

(defun itoa0 (n acc)
  (if (= n 0)
    acc
    (let (digit (% n 10)
          dchar (cond
                  (= digit 0) "0",
                  (= digit 1) "1",
                  (= digit 2) "2",
                  (= digit 3) "3",
                  (= digit 4) "4",
                  (= digit 5) "5",
                  (= digit 6) "6",
                  (= digit 7) "7",
                  (= digit 8) "8",
                  (= digit 9) "9"))
      (itoa0 (// n 10) (cons dchar acc)))))

(defun itoa (n)
  (if (= n 0)
    "0"
    (eval (cons (quote str-cat) (itoa0 n nil)))))

(defun atoi0 (in acc)
  (if in
    (let (dchar (car in)
          digit (cond
                  (equal? dchar "0") 0
                  (equal? dchar "1") 1
                  (equal? dchar "2") 2
                  (equal? dchar "3") 3
                  (equal? dchar "4") 4
                  (equal? dchar "5") 5
                  (equal? dchar "6") 6
                  (equal? dchar "7") 7
                  (equal? dchar "8") 8
                  (equal? dchar "9") 9))
      (atoi0 (cdr in) (+ digit (* 10 acc))))
    acc))

(defun atoi (int-str)
  (atoi0 (to-chars int-str) 0))

(defun string (x)
  (if (int? x)
    (itoa x)
    (string0 x)))
