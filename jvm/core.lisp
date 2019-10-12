(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote lambda) args expr)))

;; basic utilities
(defun not (p)
  (if p nil t))

(def nil? not)

(defun inc (n)
  (+ n 1))

(defun dec (n)
  (- n 1))

;(defun write-bytes (path bytes)
;  (print
;    (string "write " (length bytes) "bytes to " path)))

;(defun type (a)
;  (cond
;    (list? a)    :list
;    (int? a)     :int
;    (keyword? a) :keyword
;    (string? a)  :string
;    (char? a)    :char
;    (symbol? a)  :symbol
;    t            :unkown))

(defun list? (o) (equal? (type o) :list))
(defun int? (o) (equal? (type o) :int))
(defun keyword? (o) (equal? (type o) :keyword))
(defun string? (o) (equal? (type o) :string))
(defun symbol? (o) (equal? (type o) :symbol))
(defun char? (o) (equal? (type o) :char))

;; functional favorites

(defun map0 (fn in acc)
  (if in
    (map0 fn
          (rest in)
          (cons (fn (first in)) acc))
    acc))

(defun map (fn in)
  (reverse (map0 fn in nil)))

(defun apply (fun args)
  (eval (cons fun (map (lambda (x) (list (quote quote) x)) args))))

(defun reduce0 (fn in acc)
  (if in
    (reduce0 fn
             (rest in)
             (fn acc (first in)))
    acc))

(defun reduce (fn in)
  (reduce0 fn
           (rest (rest in))
           (fn (first in) (nth 1 in))))

(defun filter0 (pred in acc)
  (if in
    (if (not (pred (first in)))
      (filter0 pred (rest in) acc)
      (filter0 pred (rest in) (cons (first in) acc)))
  acc))

(defun filter (pred in)
  (reverse (filter0 pred in nil)))

;; list functions

(defun second (in)
  (nth 1 in))

(defun length0 (in acc)
  (if in
    (length0 (rest in) (inc acc))
    acc))

(defun length (in)
  (if (string? in)
    (length0 (to-chars in) 0)
    (length0 in 0)))

(defun append0 (in obj acc)
  (if in
    (append0 (rest in) obj (cons (first in) acc))
    (cons obj acc)))

(defun append (in obj)
  (reverse (append0 in obj nil)))

(defun nth (n in)
  (if (= n 0)
    (first in)
    (nth (dec n) (rest in))))

(defun last (in)
  (if (rest in)
    (last (rest in))
    (first in)))

(defun reverse0 (in acc)
  (if in
    (reverse0 (rest in) (cons (first in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

(defun concat (a b)
  (reverse0 (reverse a) b))

(defun index0 (needle haystack acc)
  (if haystack
    (if (equal? needle (first haystack))
      acc
      (index0 needle (rest haystack) (inc acc)))))

(defun index (needle haystack)
  (index0 needle haystack 0))

(defun repeat0 (key times acc)
  (if (< times 1)
    acc
    (repeat0 key (dec times) (cons key acc))))

(defun repeat (key times)
  (repeat0 key times nil))

;(defun flatten0 (tree acc)
;  (if tree
;    (if (list? tree)
;      (if (list? (first tree))
;        (flatten0 (rest tree) (cons (flatten0 (first tree) nil) acc))
;        (flatten0 (rest tree) (cons (first tree) acc)))
;      tree)
;    acc))

;(defun flatten (tree)
;  (reverse (flatten0 tree nil)))

;; other functions, they might be useful

(defun qsort0 (comp in pivot less same greater)
  (if in
    (let (term (first in)
          c (comp term pivot))
      (cond
        (= c 0) (qsort0 comp (rest in) pivot less (cons term same) greater)
        (< c 0) (qsort0 comp (rest in) pivot (cons term less) same greater)
        (> c 0) (qsort0 comp (rest in) pivot less same (cons term greater))))
    (list less same greater)))

(defun qsort (comp in)
  (if in
    (let (pivot (first in)
          asdf (rest in)
          terms (qsort0 comp asdf pivot nil nil nil)
          lesser (first terms)
          same (cons pivot (second terms))
          greater (nth 2 terms))
      (if (nil? asdf)
        (list pivot)
        (concat (qsort comp lesser) (concat same (qsort comp greater)))))))

(defun > (a b) (not (or (= a b) (< a b))))
(defun <= (a b) (or (< a b) (= a b)))
(defun >= (a b) (or (> a b) (= a b)))

(defun to-chars0 (s i acc)
  (if (char-at s i)
    (to-chars0 s (inc i) (cons (char-at s i) acc))
    (reverse acc)))

(defun to-chars (s)
  (to-chars0 s 0 nil))
