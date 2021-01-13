(def *gensym-count* 0)
(defun gensym (:rest prefix)
  (->> (inc *gensym-count*)
       (def *gensym-count*)
       (string (if prefix
                 (first prefix)
                 "gsym"))
       (symbol)))

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
                             (list (quote quote) (first exprs))
                             term)
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

;; basic utilities
(defun not (p)
  (if p nil t))

(defun nil? (p)
  (if p nil t))

(defun inc (n)
  (+ n 1))

(defun dec (n)
  (- n 1))

(defun print1 (objs)
  (if objs
    (progn
      (print0 (first objs))
      (pprint0 " ")
      (print1 (rest objs)))))

(defun print (:rest args)
  (progn
    (print1 args)
    (pprint0 "\n")))

(defun pprint1 (objs)
  (if objs
    (progn
      (pprint0 (first objs))
      (pprint0 " ")
      (print1 (rest objs)))))

(defun pprint (:rest args)
  (progn
    (pprint1 args)
    (pprint0 "\n")))

;(defun print (:rest args)
;  (progn
;    (map (lambda (x) (progn (print0 x) (pprint0 " "))) args)
;    (pprint0 "\n")))

;(defun pprint (:rest args)
;  (progn
;    (map (lambda (x) (progn (pprint0 x) (pprint0 " "))) args)
;    (pprint0 "\n")))

(defun get (hmap key :rest missing)
  (cond
    (nil? missing) (get0 hmap key)
    (second (get0 hmap key)) (first (get0 hmap key))
    t (first missing)))

(defun map0 (fn in acc)
  (if in
    (map0 fn
          (rest in)
          (cons (fn (first in)) acc))
    acc))

(defun map (fn in)
  (reverse (map0 fn in nil)))

;(defun apply (fun args)
;  (eval (cons fun (map (lambda (x) (list (quote quote) x)) args))))

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
  (cond
    (not in) acc
    (pred (first in)) (filter0 pred (rest in) (cons (first in) acc))
    t (filter0 pred (rest in) acc)))

(defun filter (pred in)
  (reverse (filter0 pred in nil)))

;; list functions

(defun second (in)
  (first (rest in)))

(defun third (in)
  (first (rest (rest in))))

(defun length0 (in acc)
  (if in
    (length0 (rest in) (inc acc))
    acc))

;(defun length (in)
;  (if (string? in)
;    (length0 (to-chars in) 0)
;    (length0 in 0)))

(defun length (in)
  (length0 in nil))

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
    (reverse0 (rest in) (cons0 (first in) acc))
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

;; use native flatten instead
(defun flatten0 (tree acc)
  (cond
    (nil? tree) acc
    (not (list? tree)) tree
    (list? (first tree)) (flatten0 (rest tree) (flatten0 (first tree) acc))
    t (flatten0 (rest tree) (cons (first tree) acc))))

(defun flatten (tree)
  (reverse (flatten0 tree nil)))

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

(defun string (:rest l)
  (string0 l))

(def *read-pos* 0)
(def *read-tail* 0)
(def *read-str* nil)

(defun r-peek (off)
  (if *read-str*
    (char-at *read-str* (+ *read-pos* off))))

(defun r-next! ()
  (let (x (r-peek 0))
    (progn
      (def *read-pos* (inc *read-pos*))
      x)))

(defun r-get ()
  (substr *read-str* *read-tail* *read-pos*))

(defun wait-for (ch)
  (let (x (r-peek 0))
    (cond
      (nil? x) nil
      (equal? x ch) ch
      t (progn (r-next!) (wait-for ch)))))

(defun read-atom1 (state)
  (let (ch (r-peek 0))
    (cond
      (equal? state :string)
      (cond
        (nil? ch) (print "unexpected EOF in string")

        (and (equal? ch "\\")
             (or (equal? (r-peek 1) "\"")
                 (equal? (r-peek 1) "n")
                 (equal? (r-peek 1) "\\")))
        (progn (r-next!) (r-next!) (read-atom1 state))

        (equal? ch "\"")
        (progn (r-next!) (read-atom0 (r-get)))
        
        t (progn (r-next!) (read-atom1 state)))

      (or (nil? ch) (equal? ch ")") (whitespace? ch) (equal? ch "\n"))
      (read-atom0 (r-get))

      t (progn (r-next!) (read-atom1 state)))))

(defun read-atom ()
  (let (ch (r-peek 0))
    (cond
      (equal? ch "\"") (progn (r-next!) (read-atom1 :string))
      t (read-atom1 nil))))

(defun read-form ()
  (let (ch (r-peek 0))
  (cond
    (nil? ch) nil
    (whitespace? ch) (progn (r-next!) (read-form))
    (equal? ch ";") (progn (wait-for "\n") (read-form))

    (equal? ch "'") (progn (r-next!) (list (quote quote) (read-form)))

    (equal? ch "(") (progn (r-next!) (read-list nil))
    (equal? ch ")") (print "unexpected )")
    t (progn (def *read-tail* *read-pos*) (read-atom)))))

(defun read-list (acc)
  (let (ch (r-peek 0))
    (cond
      (nil? ch) (print "unexpected eof")
      (equal? ch ")") (progn (r-next!) (reverse acc))
      t (read-list (cons (read-form) acc)))))

(defun read (str)
  (progn
    (if str
      (progn
        (def *read-str* str)
        (def *read-pos* 0)))
    (read-form)))

(defun read-all0 (acc)
  (let (sexpr (read nil))
    (if sexpr
      (read-all0 (cons sexpr acc))
      (reverse acc))))

(defun read-all (path)
  (read-all0 (list (read (slurp path)))))

(defun interleave0 (lista listb acc)
  (cond
    (equal? :rest (first lista))
    (reverse (cons listb (cons (second lista) acc)))

    (and lista listb)
    (interleave0 (rest lista) (rest listb)
                 (cons (first listb) (cons (first lista) acc)))

    (or lista listb)
    (progn (print lista) (print listb) (print acc)
    (print "error: unmatched interleave arguments"))

    t (reverse acc)))

(defun interleave (lista listb)
  (interleave0 lista listb nil))

(defun apply-progn (exprs env)
  (if (rest exprs)
    (progn
      (apply (first exprs) env)
      (apply-progn (rest exprs) env))
    (apply (first exprs) env)))

;; TODO: check for even number of val/bindings
(defun let-bind (exprs env)
  (if exprs
    (progn
      (insert! env (first exprs) (apply (second exprs) env))
      (let-bind (rest (rest exprs)) env))
    env))

;; TODO: check for even number of val/bindings
(defun apply-cond (exprs env)
  (cond
    (not exprs) nil
    (apply (first exprs) env) (apply (second exprs) env)
    t (apply-cond (rest (rest exprs)) env)))

(defun apply-and (exprs env)
  (cond
    (not exprs) t
    (apply (first exprs) env) (apply-and (rest exprs) env)
    t nil))

(defun apply-or (exprs env)
  (cond
    (not exprs) nil
    (apply (first exprs) env) t
    t (apply-or (rest exprs) env)))

;; TODO: check for even number of val/bindings
(defun lambda-bind (exprs env)
  (if exprs
    (progn
      (insert! env (first exprs) (second exprs))
      (lambda-bind (rest (rest exprs)) env));)
    env))

(defun lambda-apply (func args env)
  (let (;_ (print func)
        ;_ (print args)
        sym-binds (interleave (get-args func) args)
        bind-envir (lambda-bind sym-binds (make-envir env)))
    (apply (get-expr func) bind-envir)))

(defun macro-call? (ast env)
  (and (list? ast)
       (contains? env (first ast))
       (macro? (first (get env (first ast))))))

(defun macro-expand (ast env)
  (if (macro-call? ast env)
    (macro-expand
      (lambda-apply (first (get env (first ast))) (rest ast) env)
      env)
    ast))

(defun eval (ast env acc)
  (cond
    (not ast) (reverse acc)
    (symbol? ast) (first (get env ast))
    (not (list? ast)) ast
   
    t (eval (rest ast) env (cons (apply (first ast) env) acc))))

(defun apply (ast env)
  (cond
    (not ast) nil

    (macro-call? ast env)
    (apply (macro-expand ast env) env)

    (not (list? ast)) (eval ast env nil)

    ;; TODO: throw instead of print errors

    ;; if
    (equal? (first ast) (quote if))
    (cond
      (apply (second ast) env) (apply (nth 2 ast) env)
      (= (length ast) 4) (apply (nth 3 ast) env)
      (= (length ast) 3) nil
      t (print "if expects two or three arguments"))

    ;; quote
    (equal? (first ast) (quote quote))
    (if (= (length ast) 2)
      (second ast)
      (print "quote expects one argument"))

    ;; def
    (equal? (first ast) (quote def))
    (if (= (length ast) 3)
      (let (val (apply (nth 2 ast) env))
        (progn
          (insert! (user-envir) (second ast) val)
          val))
      (print "def expects two arguments"))

    ;; progn
    (equal? (first ast) (quote progn))
    (apply-progn (rest ast) env)

    ;; let
    (equal? (first ast) (quote let))
    (let (bind-envir (let-bind (second ast) (make-envir env)))
      (if (= (length ast) 3)
        (apply (nth 2 ast) bind-envir)
        (print "let expects two arguments")))

    ;; defmacro
    (equal? (first ast) (quote defmacro))
    (if (= (length ast) 4)
      (let (val (make-macro (nth 2 ast) (nth 3 ast)))
        (progn
          (insert! env (second ast) val)
          val))
      (print "defmacro expects four arguments"))

    ;; lambda
    (equal? (first ast) (quote lambda))
    (if (= (length ast) 3)
      (make-lambda (second ast) (nth 2 ast))
      (print "lambda expects two arguments"))

    ;; cond
    (equal? (first ast) (quote cond))
    (apply-cond (rest ast) env)

    ;; and
    (equal? (first ast) (quote and))
    (apply-and (rest ast) env)

    ;; or
    (equal? (first ast) (quote or))
    (apply-or (rest ast) env)

    t
    (let (eval-list (eval ast env nil))
      (cond
        (native-fn? func) (native-invoke func args)
        (lambda? func)    (lambda-apply func args env)
        t                 (print "error: " func " is not a function")))))

(defun invoke (func args)
  (cond
    (native-fn? func) (native-invoke func args)
    (lambda? func)    (lambda-apply func args (user-envir))
    t (print "error: " func " is not a function")))

(defun main ()
  (progn
    (pprint "user>> ")
    (print (apply (read (readline)) (user-envir)))
    (main)))
