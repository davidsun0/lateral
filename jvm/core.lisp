; runtime list
; TODO: move to Runtime.java
(def list (lambda (:rest l) l))

(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote lambda) args expr)))

;(defmacro defun (name args expr)
;  `(def ,name (lambda ,args ,expr))

; TODO: move to Runtime.java
(defun cons1 (in acc)
  (if in
    (cons1 (rest in) (cons0 (first in) acc))
    acc))

; TODO: move to Runtime.java
(defun cons (:rest l)
  (let (rl (reverse l))
    (cons1 (rest rl) (first rl))))

;; clojure threading macros
(defun ->0 (exprs acc)
  (if exprs
    (->0 (rest exprs)
         (cons (first (first exprs))
               acc
               (rest (first exprs))))
    acc))

(defun ->>0 (exprs acc)
  (if exprs
    (->>0 (rest exprs) (append (first exprs) acc))
    acc))

(defmacro -> (:rest exprs)
  (->0 (rest exprs) (first exprs)))

(defmacro ->> (:rest exprs)
  (->>0 (rest exprs) (first exprs)))

;; convinience macros
(defun case0 (term exprs acc)
  (cond
    (not exprs) (reverse acc)
    (nil? (rest exprs)) (reverse (cons (first exprs) t acc))
    t (case0 term
             (rest (rest exprs))
             (cons (second exprs)
                   (cons (list (quote equal?) (first exprs) term)
                         acc)))))

;; bug when nil in a case list
(defun case0 (term exprs acc)
  (cond
    (not exprs) (reverse acc)

    ;; append else clause
    (nil? (rest exprs)) (reverse (cons (first exprs) t acc))

    ;; list to match
    (list? (first exprs))
    (case0 term
           (rest (rest exprs))
           (cons (second exprs)
                 (cons (list (quote index)
                             term
                             (list (quote quote) (first exprs)))
                       acc)))

    ;; single term
    t (case0 term
             (rest (rest exprs))
             (cons (second exprs)
                   (cons (list (quote equal?) (first exprs) term)
                         acc)))))

;; TODO: wrap in let and only eval term once
(defun case1 (terms)
  (let (val (gensym))
    (list (quote let)
          (list val (first terms))
          (cons (quote cond)
            (case0 val
                   (rest terms)
                   nil)))))

(defmacro case (:rest terms)
  (case1 terms))

(defun print (:rest args)
  (print1 args))

(defun pprint (:rest args)
  (pprint1 args))

(defun string (:rest args)
  (string0 args))

(defun print-iden (x)
  (progn
    (print x)
    x))

; optional arguments?
(defun get (hmap key :rest missing)
  (cond
    (nil? missing) (get0 hmap key)
    (second (get0 hmap key)) (first (get0 hmap key))
    t (first missing)))

(defun char? (o) (equal? (type o) :char))

;; functional favorites

(defun map0 (fn in acc)
  (if in
    (map0 fn (rest in) (cons (fn (first in)) acc))
    acc))

(defun map (fn in)
  (reverse (map0 fn in nil)))

(defun apply (fun args)
  (eval (cons fun (map (lambda (x) (list (quote quote) x)) args))))

(defun reduce0 (fn in acc)
  (if in
    (reduce0 fn (rest in) (fn acc (first in)))
    acc))

(defun reduce (fn in)
  (reduce0 fn
           (rest (rest in))
           (fn (first in) (nth 1 in))))

(defun filter0 (pred in acc)
  (cond
    (not in) acc
    (pred (first in)) (filter0 pred (rest in) (cons (first in) acc))
    t (filter0 pred (rest in) acc)))

(defun filter (pred in)
  (reverse (filter0 pred in nil)))

(defun foldl (fun acc in)
  (if in
    (foldl fun (fun acc (first in)) (rest in))
    acc))

(defun max (:rest in)
  (if (rest in)
    (reduce (lambda (a b) (if (< a b) b a)) in)
    (first in)))

;; list functions
(defun concat0 (head llist acc)
  (cond
    (and (nil? head) (nil? llist)) (reverse acc)
    (nil? head) (concat0 (first llist) (rest llist) acc)
    t (concat0 (rest head) llist (cons (first head) acc))))

(defun concat (:rest args)
  (concat0 (first args) (rest args) nil))

(defun last (in)
  (if (rest in)
    (last (rest in))
    (first in)))

(defun reverse0 (in acc)
  (if in
    (reverse0 (rest in) (cons0 (first in) acc))
    acc))

(defun repeat0 (key times acc)
  (if (< times 1)
    acc
    (repeat0 key (dec times) (cons key acc))))

(defun repeat (key times)
  (repeat0 key times nil))

;; other functions, they might be useful
(defun split0 (lst n acc)
  (if (> n 0)
    (split0 (rest lst) (dec n) (cons (first lst) acc))
    (list (reverse acc) lst)))

(defun msort0 (comp a b acc)
  (cond
    (and (nil? a) (nil? b)) (reverse acc)
    (nil? b) (msort0 comp (rest a) b (cons (first a) acc))
    (nil? a) (msort0 comp a (rest b) (cons (first b) acc))

    (comp (first a) (first b)) (msort0 comp (rest a) b (cons (first a) acc))
    t (msort0 comp a (rest b) (cons (first b) acc))))

(defun msort (comp in)
  (let (len (length in)
        halves (split0 in (// len 2) nil))
    (if (<= len 1)
      in
      (msort0 comp
              (msort comp (first halves))
              (msort comp (second halves))
              nil))))

(defun > (a b) (not (or (= a b) (< a b))))
(defun <= (a b) (or (< a b) (= a b)))
(defun >= (a b) (or (> a b) (= a b)))

(defun to-chars0 (s i acc)
  (if (char-at s i)
    (to-chars0 s (inc i) (cons (char-at s i) acc))
    (reverse acc)))

(defun to-chars (s)
  (to-chars0 s 0 nil))
