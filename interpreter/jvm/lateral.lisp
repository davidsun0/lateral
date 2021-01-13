(defun not (p)
  (if p nil t))

(defun nil? (p)
  (if p nil t))

(defun inc (n)
  (add0 1 n))

(defun dec (n)
  (add0 n (negate 1)))

(defun add1 (nums acc)
  (if nums
    (add1 (rest nums) (add0 (first nums) acc))
    acc))

(defun + (:rest nums)
  (add1 nums 0))

(defun - (:rest nums)
  (if (rest nums)
    (add0 (first nums) (negate (add1 (rest nums) 0)))
    (negate (first nums))))

(defun second (lst)
  (first (rest lst)))

(defun third (lst)
  (first (rest (rest lst))))

(defun fourth (lst)
  (first (rest (rest (rest lst)))))

(defun nth (n list)
  (if (= n 0)
    (first list)
    (nth (dec n) (rest list))))

(defun length0 (lst acc)
  (if lst
    (length0 (rest lst) (inc acc))
    acc))

(defun length (lst)
  (length0 lst 0))

(defun reverse0 (in acc)
  (if in
    (reverse0 (rest in) (cons0 (first in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

(defun concat (a b)
  (reverse0 (reverse a) b))

(defun index0 (needle haystack acc)
  (cond
    (nil? haystack) nil
    (equal? needle (first haystack)) acc
    t (index0 needle (rest haystack) (inc acc))))

(defun index (needle haystack)
  (index0 needle haystack 0))

(defun last-index0 (needle haystack curr idx)
  (cond
    (nil? haystack) idx

    (equal? needle (first haystack))
    (last-index0 needle (rest haystack) (inc curr) curr)

    t (last-index0 needle (rest haystack) (inc curr) idx)))

(defun last-index (needle haystack)
  (last-index0 needle haystack 0 nil))

(defun append (in obj)
  (reverse (cons obj (reverse in))))

(defun repeat0 (key times acc)
  (if (< times 1)
    acc
    (repeat0 key (dec times) (cons key acc))))

(defun repeat (key times)
  (repeat0 key times nil))

(defun map0 (fn in acc)
  (if in
    (map0 fn (rest in) (cons (fn (first in)) acc))
    acc))

(defun map (fn in)
  (reverse (map0 fn in nil)))

(defun filter0 (pred in acc)
  (if in
    (filter0 pred (rest in)
             (if (pred (first in))
               (cons (first in) acc)
               acc))
    acc))

(defun filter (pred in)
  (reverse (filter0 pred in nil)))

(defun foldl (fun acc in)
  (if in
    (foldl fun (fun acc (first in)) (rest in))
    acc))

(defun flatten0 (tree acc)
  (cond
    (nil? tree) acc
    (not (list? tree)) tree
    (list? (first tree)) (flatten0 (rest tree) (flatten0 (first tree) acc))
    t (flatten0 (rest tree) (cons (first tree) acc))))

(defun flatten (tree)
  (reverse (flatten0 tree nil)))

(defun print1 (in)
  (if in
    (progn (print0 (first in))
           (pprint0 " ")
           (print1 (rest in)))
    (pprint0 "\n")))

(defun print (:rest args)
  (print1 args))

(defun pprint1 (in)
  (if in
    (progn (pprint0 (first in))
           (pprint0 " ")
           (print1 (rest in)))
    (pprint0 "\n")))

(defun pprint (:rest args)
  (pprint1 args))

(defun to-chars0 (s i acc)
  (if (char-at s i)
    (to-chars0 s (inc i) (cons (char-at s i) acc))
    (reverse acc)))

(defun to-chars (s)
  (to-chars0 s 0 nil))

(defun string (:rest s)
  (string0 s))

(defun get (hmap key :rest missing)
  (let (res (get0 hmap key))
    (cond
      (nil? missing) res
      (second res)   (first res)
      t              (first missing))))

(defun > (a b) (not (or (= a b) (< a b))))

(defun <= (a b) (or (< a b) (= a b)))

(defun >= (a b) (or (> a b) (= a b)))

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

(def *gensym-count* 0)
(defun gensym (:rest prefix)
  (symbol
    (string0 
      (list (if prefix (first prefix) "gsym")
            (def *gensym-count*
                 (inc *gensym-count*))))))

(defun ->>0 (exprs acc)
  (if exprs
    (->>0 (rest exprs) (append (first exprs) acc))
    acc))

(defmacro ->> (:rest exprs)
  (->>0 (rest exprs) (first exprs)))

;; bug when nil in a case list
(defun case0 (term exprs acc)
  (cond
    (nil? exprs) (reverse acc)

    ;; append else clause
    (nil? (rest exprs)) (reverse (cons (first exprs) 't acc))

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

;(defun case1 (terms)
;  (let (val (gensym "case-"))
;    `(let (,val ,(first terms))
;       ,(cons 'cond (case0 val (rest terms) nil)))))

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

;;=============================================================================
;; READ FUNCTIONS
;;=============================================================================

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
      (nil? ch)         nil
      (whitespace? ch)  (progn (r-next!) (read-form))
      (equal? ch ";")   (progn (wait-for "\n") (read-form))
      (equal? ch "'")   (progn (r-next!) (list (quote quote) (read-form)))
      (equal? ch "(")   (progn (r-next!) (read-list nil))
      (equal? ch ")")   (print "unexpected )")
      t                 (progn (def *read-tail* *read-pos*) (read-atom)))))

(defun read-list (acc)
  (let (ch (r-peek 0))
    (cond
      (nil? ch)         (print "unexpected EOF")
      (whitespace? ch)  (progn (r-next!) (read-list acc))
      (equal? ch ";")   (progn (wait-for "\n") (read-list acc))
      (equal? ch ")")   (progn (r-next!) (reverse acc))
      t                 (read-list (cons (read-form) acc)))))

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

;;=============================================================================
;; INTERPRETER
;;=============================================================================

(defun interleave0 (lista listb acc)
  (cond
    (equal? :rest (first lista))
    (reverse (cons listb (second lista) acc))

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
      (apply0 (first exprs) env)
      (apply-progn (rest exprs) env))
    (apply0 (first exprs) env)))

;; TODO: check for even number of val/bindings
(defun let-bind (exprs env)
  (if exprs
    (progn
      (insert! env (first exprs) (apply0 (second exprs) env))
      (let-bind (rest (rest exprs)) env))
    env))

;; TODO: check for even number of val/bindings
(defun apply-cond (exprs env)
  (cond
    (not exprs) nil
    (apply0 (first exprs) env) (apply0 (second exprs) env)
    t (apply-cond (rest (rest exprs)) env)))

(defun apply-and (exprs env)
  (cond
    (not exprs) t
    (apply0 (first exprs) env) (apply-and (rest exprs) env)
    t nil))

(defun apply-or (exprs env)
  (cond
    (not exprs) nil
    (apply0 (first exprs) env) t
    t (apply-or (rest exprs) env)))

;; TODO: check for even number of val/bindings
(defun lambda-bind (exprs env)
  (if exprs
    (progn
      (insert! env (first exprs) (second exprs))
      (lambda-bind (rest (rest exprs)) env))
    env))

(defun lambda-apply (func args env)
  (let (sym-binds (interleave (get-args func) args)
        bind-envir (lambda-bind sym-binds (make-envir env)))
    (apply0 (get-expr func) bind-envir)))

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

(defun eval0 (ast env acc)
  (cond
    (not ast) (reverse acc)
    (symbol? ast) (first (get env ast))
    (not (list? ast)) ast
    t (eval0 (rest ast) env (cons (apply0 (first ast) env) acc))))

(defun apply0 (ast env)
  (cond
    (not ast) nil

    (macro-call? ast env)
    (apply0 (macro-expand ast env) env)

    (not (list? ast)) (eval0 ast env nil)

    ;; TODO: throw instead of print errors

    ;; if
    (equal? (first ast) (quote if))
    (cond
      (apply0 (second ast) env) (apply0 (third ast) env)
      (= (length ast) 4) (apply0 (fourth ast) env)
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
      (let (val (apply0 (third ast) env))
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
        (apply0 (third ast) bind-envir)
        (print "let expects two arguments")))

    ;; defmacro
    (equal? (first ast) (quote defmacro))
    (if (= (length ast) 4)
      (let (val (make-macro (third ast) (fourth ast)))
        (progn
          (insert! env (second ast) val)
          val))
      (print "defmacro expects four arguments"))

    ;; lambda
    (equal? (first ast) (quote lambda))
    (if (= (length ast) 3)
      (make-lambda (second ast) (third ast))
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
    (let (eval-list (eval0 ast env nil)
          func (first eval-list))
      (cond
        (native-fn? func) (native-invoke func (rest eval-list))
        (lambda? func)    (lambda-apply func (rest eval-list) env)
        t                 (print "error: " func " isn't a function")))))

(defun lambda-invoke (fn args)
  (cond
    (native-fn? fn) (native-invoke fn args)
    (lambda? fn) (lambda-apply fn args (user-envir))
    t (print "can't invoke " fn " as lambda")))

(defun eval (ast)
  (apply0 ast (user-envir)))

(defun include (path)
  (eval (cons (quote progn) (read-all path))))

;(defun apply (fn args)
;  (->> args
;       (map (lambda (x) (list 'quote x)))
;       (cons fun)
;       (eval)))

(include "core.lisp")
(defun main ()
  (progn
    (pprint0 "user> ")
    (print (apply0 (read (readline)) (user-envir)))
    (main)))

;;=============================================================================
;; COMPILER
;;=============================================================================

(defun quote-flatten0 (ast acc)
  (cond
    (nil? ast) (reverse acc)

    (list? ast)
    (quote-flatten0 (rest ast)
                    (concat (quote-flatten (first ast)) acc))

    t (list (list :push
            (cond
              (symbol? ast) :symbol
              (keyword? ast) :keyword
              (string? ast) :str-const
              (int? ast) :int-const
              t (print "can't quote flatten " ast))
            ast))))

(defun quote-flatten (ast)
  (semi-flatten
    (if (list? ast)
      (concat (quote-flatten0 ast nil)
            (list (list :funcall 'list :argc (length ast))))
      (quote-flatten0 ast nil))))

(defun closure-flatten0 (args locals ast acc)
  (cond
    (nil? ast) (reverse acc)

    (list? ast)
    (closure-flatten0
      args
      locals
      (rest ast)
      (cons (closure-flatten1 args locals (first ast)) acc))

    (index ast args) (list (list :push :symbol ast))
    (index ast locals) (list (list :push :arg-num (index ast locals)))

    (int? ast) (list (list :push :int-const ast))
    (string? ast) (list (list :push :str-const))
    (keyword? ast) (list (list :push keyword-const))

    (symbol? ast)
    (list (list :push :symbol 'quote)
          (list :push :symbol ast)
          (list :funcall 'envir-get :argc 1)
          (list :funcall 'list :argc 2))

    t (print "closure-flatten0 can't handle " ast)))

(defun closure-flatten1 (args locals ast)
  (if (list? ast)
    (list (closure-flatten0 args locals ast nil)
          (list :funcall 'list :argc (length ast)))
    (closure-flatten0 args locals ast nil)))

(defun closure-flatten (args locals ast)
   (list (list :funcall 'make-lambda :argc 2)
         (reverse (semi-flatten (closure-flatten1 args locals ast)))
         (reverse (quote-flatten args))))

;;; reduces a tree to a list of lists
(defun semi-flatten0 (in acc)
  (cond
    (nil? in) acc

    (and (list? (first in)) (list? (first (first in))))
    (semi-flatten0 (rest in) (semi-flatten0 (first in) acc))

    t (semi-flatten0 (rest in) (cons (first in) acc))))

(defun semi-flatten (in)
  (reverse (semi-flatten0 in nil)))

;; macro for x-deflate
;`(defun ,fname (name args expr acc ,@(vars))
;   (if expr
;     (,fname name args 
;             ,expr-mod
;             ,acc-mod
;             ,@(vars-mod))
;     (cons ,acc-add acc)))

(defun progn-deflate (name args expr acc)
  (if expr
    (progn-deflate
      name args
      (rest expr)
      (cons
        (list
          (if (rest expr)
            (list :pop)
            (cons nil nil))
          (ir0 (if (rest expr) nil name)
               args
               (first expr) nil))
         acc))
    acc))

(defun or-deflate (name args end-lab expr acc)
  (if expr
    (or-deflate name args end-lab
                (rest expr)
                (cons
                (list (list :pop)
                      (list :jump-not-nil end-lab)
                      (list :dup)
                      (ir0 nil args (first expr) nil))
                acc))
    (cons (list (if name
                  (list :return)
                  (cons nil nil))
                (list :label end-lab)
                (list :push :nil))
          acc)))

(defun and-deflate (name args false-lab expr acc)
  (if expr
    (and-deflate name args
                 false-lab
                 (rest expr)
                 (cons (list (if (rest expr)
                               (list :jump-if-nil false-lab)
                               (cons nil nil))
                             (ir0 nil args (first expr) nil))
                       acc))
    (let (end-lab (gensym "and-e"))
      (cons (list (if name
                    (list :return)
                    (cons nil nil))
                  (list :label end-lab)
                  (list :push :nil)
                  (list :label false-lab)
                  (list :goto end-lab))
            acc))))

(defun cond-deflate (name args expr test-lab end-lab acc)
  (if expr
    (let (test     (first expr)
          branch   (second expr)
          next-lab (gensym "cond-"))
      (cond-deflate name args
                    (rest (rest expr)) next-lab end-lab
                    (cons (list (if name
                                  (cons nil nil)
                                  (list :goto end-lab))
                                (ir0 name args branch nil)
                                (list :jump-if-nil next-lab)
                                (ir0 nil args test nil)
                                (if test-lab
                                  (list :label test-lab)
                                  (cons nil nil)))
                          acc)))
    (cons (list (if name
                  (list :return)
                  (list nil))
                (list :label end-lab)
                (list :push :nil)
                (list :label test-lab))
          acc)))

(defun let-deflate0 (args largs bind-list acc)
  (if bind-list
    (let (largs-new (if (index (first bind-list) largs)
                      largs
                      (append largs (first bind-list))))
      (let-deflate0 args
                    largs-new
                    (rest (rest bind-list))
                    (cons (list :store (+ (length args)
                                          (index (first bind-list) largs-new)))
                          (ir0 nil (concat args largs) (second bind-list) nil)
                          acc)))
    (list (concat args largs) acc)))

(defun let-deflate (name args expr)
  (let (x (let-deflate0 args nil (first expr) nil)
        largs (first x)
        bind-ir (second x))
  (list
    (list :let-pop (gensym "letp") (length args))
    (ir0 name largs (second expr) nil)
    (list :local-count (gensym "letc") (length largs))
    bind-ir)))

;; iterates along list, resolving nested lists with ir0
(defun ir1 (args ast acc)
  (cond
    (nil? ast) acc

    (list? (first ast))
    (ir1 args (rest ast)
         (concat (ir0 nil args (first ast) nil) acc))

    t
    (ir1 args (rest ast)
         (cons
           (if (symbol? (first ast))
             (cond
               (equal? (first ast) (quote nil)) (list :push :nil)
               (equal? (first ast) (quote t)) (list :push :true)

               (last-index (first ast) args)
               (list :push :arg-num (last-index (first ast) args))

               t
               (list (list :funcall :envir-get :argc 1)
                     (list :push :symbol (first ast))))
           (list
             :push
             (cond
               (int? (first ast)) :int-const
               (string? (first ast)) :str-const
               (keyword? (first ast)) :keyword
               (equal? (first ast) (quote t)) :true
               ;; TODO fix bug in compile-time macro expansion that generates t
               (equal? (first ast) t) :true
               (equal? (first ast) (quote nil)) :nil
               t :unknown)
             (first ast)))
           acc))))

;; first step in code processing
;; turns a tree of lisp code into a stack-based intermediate representation
;; name doubles as a flag for if the expression is in the tail position
(defun ir0 (name args ast acc)
  (cond
    (nil? ast) (reverse acc)

    (not (list? ast))
    (list (if name (list :return) (list nil))
          (ir1 args (list ast) nil))

    (equal? (first ast) (quote if))
    (let (false-label (gensym "if-f")
          end-label (gensym "if-e"))
      (list
        (if (not name) (list :label end-label) (cons nil nil))
        (if (= (length ast) 4)
          ;; has an else branch
          (ir0 name args (nth 3 ast) nil)
          ;; no else branch
          (list (if name
                  (list :return)
                  (cons nil nil))
                (list :push :nil)))
        (list :label false-label)
        (if (nil? name)
          (list :goto end-label)
          (list nil))
        (ir0 name args (third ast) nil)
        (list :jump-if-nil false-label)
        (ir0 nil args (second ast) nil)))

    (equal? (first ast) (quote and))
    (and-deflate name args (gensym "and-f") (rest ast) nil)

    (equal? (first ast) (quote or))
    (or-deflate name args (gensym "or-e") (rest ast) nil)

    (equal? (first ast) (quote cond))
    (cond-deflate name args (rest ast) nil (gensym "cond-e") nil)

    (equal? (first ast) (quote progn))
    (progn-deflate name args (rest ast) nil)

    (equal? (first ast) (quote let))
    (let-deflate name args (rest ast))

    (equal? (first ast) (quote quote))
    (list (if name (list :return) (list nil))
          (reverse (quote-flatten (second ast))))

    (equal? (first ast) (quote lambda))
    (list (if name (list :return) (list nil))
          (closure-flatten (second ast) args (third ast)))

    ;; TODO: check arg is symbol
    (equal? (first ast) (quote def))
    (list (if name (list :return) (list nil))
          (list :funcall 'envir-set :argc 2)
          (ir0 nil args (third ast) nil)
          (list :push :symbol (second ast)))

    (equal? (first ast) name)
    (cons (list :tail-recur :argc (dec (length ast)))
          (ir1 args (rest ast) nil))

    name (cons (list :return) (ir0 nil args ast acc))

    (last-index (first ast) args)
    (cons (list :dynamcall :argc (length ast))
          (ir1 args ast nil))

    t
    (cons (list :funcall (first ast) :argc (dec (length ast)))
          (ir1 args (rest ast) nil))))

(defun ir (name args ast)
  (->> (ir0 name args ast nil)
       (semi-flatten)
       (reverse)
       (filter first)))

(defun u1 (x)
  (if (< x 0x100)
    (list x)))

(defun u2 (x)
  (if (or (< x 0xFFFF) (= x 0xFFFF))
    (list (bit-and (bit-asr x 8) 0xFF)
          (bit-and x 0xFF))))

(defun u4 (x)
  (concat (u2 (// x 0x10000))
          (u2 (bit-and x 0xFFFF))))

(defun len1-attribs (const-list)
  (u2 (second const-list)))

(defun len2-attribs (const-list)
  (concat (u2 (second const-list))
          (u2 (third const-list))))

;const-to-bin
(defun const-to-bin (const-list)
  (case (first const-list)
    :utf8 (cons 0x01 (u2 (length (to-chars (second const-list))))
                (string-bytes (second const-list)))
    :integer     (cons 0x03 (u4 (second const-list)))
    :classref    (cons 0x07 (len1-attribs const-list))
    :string      (cons 0x08 (len1-attribs const-list))
    :fieldref    (cons 0x09 (len2-attribs const-list))
    :methodref   (cons 0x0A (len2-attribs const-list))
    :nametyperef (cons 0x0C (len2-attribs const-list))
    (list :unknown const-list)))

(defun pool-search! (constpool expr)
  (if (contains? constpool expr)
    (first (get constpool expr))
    (let (pool-count (get constpool :count 1))
      (progn
        (insert! constpool expr pool-count)
        (insert! constpool :count (inc pool-count))
        pool-count))))

(defun pool-get! (constpool expr)
  (let (expr (if (string? expr) (list :utf8 expr) expr)
        tag (first expr))
    (case tag
      (:utf8 :integer)
      (pool-search! constpool expr)

      (:classref :string)
      (pool-search!
        constpool
        (list tag (pool-get! constpool (string (second expr)))))

      (:nametyperef :methodref :fieldref)
      (pool-search!
        constpool
        (list tag
              (pool-get! constpool (second expr))
              (pool-get! constpool (third expr))))

      :getstatic
      (pool-search!
        constpool
        (list tag
              (pool-get! constpool (second expr))
              (pool-get! constpool (third expr))
              (pool-get! constpool (fourth expr))))

      (print "can't pool-get" expr tag "test"))))

; generates code to store stack onto local args for tail recursion
(defun set-locals (n i acc)
  (if (< n i)
    acc
    (set-locals n (inc i) (cons (list :astore i) acc))))

(defun ir-to-jvm (expr)
    (case (first expr)
      :return (list :areturn)

      :push
      (case (nth 1 expr)
        :arg-num (list :aload (nth 2 expr))
        :nil     (list :aconst_null)
        :true    (list :getstatic
                       "java/lang/Boolean"
                       "TRUE"
                       "Ljava/lang/Boolean;")
        :int-const
        (list (list :iconst (nth 2 expr))
              (list :invokestatic "java/lang/Integer"
                    "valueOf" "(I)Ljava/lang/Integer;"))

        :char-const
        (list (list :iconst (integer (nth 2 expr)))
              (list :invokestatic "java/lang/Character"
                    "valueOf" "(C)Ljava/lang/Character;"))

        :str-const
        (list :ldc (list :string (nth 2 expr)))

        :symbol
        (list (list :ldc (list :string (string (nth 2 expr))))
              (list :invokestatic "Symbol" "makeSymbol"
                    "(Ljava/lang/String;)LSymbol;"))

        :keyword
        (list (list :ldc (list :string (string (nth 2 expr))))
              (list :invokestatic "Keyword" "makeKeyword"
                    "(Ljava/lang/String;)LKeyword;"))

       (print "ir-to-jvm: can't push" expr))

      :store        (list :astore (second expr))
      :jump-if-nil  (cons :ifnull (rest expr))
      :jump-not-nil (cons :ifnonnull (rest expr))
      :tail-recur   (set-locals (dec (nth 2 expr)) 0 (list (list :goto :start)))
      :dynamcall    (list (list :funcall 'list :argc (dec (third expr)))
                          (cons :funcall 'invoke :argc (rest expr)))
      expr))

;; prepends start label to ir if there are tail recursive calls
(defun check-tco0 (in curr)
  (cond
    (nil? curr) in
    (equal? (first (first curr)) :tail-recur) (cons (list :label :start) in)
    t (check-tco0 in (rest curr))))

(defun check-tco (in)
  (check-tco0 in in))

(defun funcall-resolve (method-list expr)
  (if (equal? (first expr) :funcall)
    (let (call (->> (second expr)
                    (string)
                    (get method-list)
                    (first)))
      (cond
        (nil? call) (print "funcall-resolve can't resolve " call expr)
        (lambda? call) (call method-list (fourth expr))
        (list? call) (cons :invokestatic call)
        t (list :invokestatic call)))
    expr))

;; converts a single item of the form (:jvmcode args) into a list of bytes
(defun jvm-assemble (in)
  (case (first in)
    ;; simple bytecode ops
    (:aconst_null :pop :dup :areturn :return :nop)
    (list (get *bytecodes* (first in) nil))

    :aload
    (if (< (second in) 4)
      (list (+ 0x2A (second in)))
      (list 0x19 (second in)))

    :astore
    (if (< (second in) 4)
      (list (+ 0x4B (second in)))
      (list 0x3A (second in)))

    :iconst
    ; iconst_<> -> bipush -> sipush -> ldc
    (let (val (second in))
      (cond
        ; iconst literal
        (and (< (- 2) val) (< val 6))       (list (+ val 3))
        ; bipush
        (and (< (- 129) val) (< val 128))   (list 0x10 val)
        t (list :ldc (list :integer val))))
    in))

(defun pool-resolve! (pool in)
  (case (first in)
    (:invokestatic :invokevirtual)
    (->> (list :methodref 
               (list :classref
                     (second in))
               (list :nametyperef
                     (third in)
                     (fourth in)))
         (pool-get! pool)
         (u2)
         (cons (first (get *bytecodes* (first in)))))

    :getstatic
    (->> (nth 3 in)
         (list :nametyperef (third in))
         (list :fieldref
               (list :classref (second in)))
         (pool-get! pool)
         (u2)
         (cons 0xB2))

    :ldc
    (let (idx (pool-get! pool (second in)))
      (if (< idx 0x100)
        (cons 0x12 (u1 idx))
        (cons 0x13 (u2 idx))))

    :checkcast
    (->> (second in)
         (list :classref)
         (pool-get! pool)
         (u2)
         (cons 0xC0))
    in))

;; calculates label byte offsets
(defun label-resolve0! (a b)
  (let (labelmap (first a)
        offset   (second a))
    (if (index (first b) (list :label :local-count :let-pop))
      (list (insert! labelmap (second b) offset)
            offset)
      (list labelmap
            (+ offset
               (cond
                 (int? (first b)) (length b)
                 (index (first b) (list :ifnull :ifnonnull :goto)) 3
                 t (print "unknown label: " b)))))))

(defun label-resolve (bytes)
  (first (foldl label-resolve0! (list (hashmap) 0) bytes)))

;; resolves jumps, associates labels with byte offsets
(defun jump-resolve0 (in acc offset labelmap)
  (let (expr (first in)
        cmd  (first expr))
    (cond
      (nil? in) (reverse acc)

      (index cmd (list :ifnull :ifnonnull :goto))
      (jump-resolve0 (rest in)
                     (cons (cons
                             (get *bytecodes* cmd nil)
                             (u2 (- (get labelmap (second expr) nil) offset)))
                           acc)
                     (+ offset 3)
                     labelmap)

      t (jump-resolve0 (rest in)
                       (cons expr acc)
                       (+ offset
                          (if (int? cmd) (length expr) 0))
                       labelmap))))

(defun jump-resolve (jvm-asm)
  (->> (label-resolve jvm-asm)
       (jump-resolve0 jvm-asm nil 0)
       (filter (lambda (x) (int? (first x))))))

;; quick and dirty way to get argc from type string
;; should count number of matches for this regex
;; "\([ILFDC(L.*;)]*\)"
(defun count-semi (str idx acc)
  (let (c (char-at str idx))
    ;; do not convert to case. there is a bug with nil
    (cond
      (index c (list nil (to-char ")"))) acc
      (index c (list (to-char "I") (to-char ";"))) (count-semi str (inc idx) (inc acc))
      t (count-semi str (inc idx) acc))))

(defun local-info0 (acc expr)
  (let (mloc    (first acc)
        lablist (second acc))
    (case (first expr)
      ;; set based on local tag info
      (:local-count :let-pop)
      (list (third expr)
            (insert! lablist (second expr) (third expr)))

      (:ifnull :ifnonnull :goto)
      (list mloc
            (if (equal? (second expr) :start)
              lablist
              (insert! lablist (second expr) mloc)))
     
      (list mloc lablist))))

(defun local-info (argc jvm-asm)
  (insert!
    (second (foldl local-info0 (list argc (hashmap)) jvm-asm))
    :start argc))

;stack-info0

;; stack frame object entry of class Object
(defun objvar-info (pool)
  (->> "java/lang/Object"
       (list :classref)
       (pool-get! pool)
       (u2)
       (cons 0x07)))

(defun sframe1 (offset last-local l-count s-count)
  ;; https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.7.4
  (let (obj (objvar-info pool))
    (cond
      ;; same frame
      (and (= s-count 0) (= last-local l-count) (< offset 64))
      (list offset)

      ;; same frame extended
      (and (= s-count 0) (= last-local l-count))
      (list 251 (u2 offset))

      ;; same locals 1 stack item
      (and (= s-count 1) (= last-local l-count) (< offset 64))
      (list (+ offset 64) obj)

      ;; same locals 1 stack item extended
      (and (= s-count 1) (= last-local l-count))
      (list 247 (u2 offset) obj)

      ;; chop frame
      (and (= s-count 0) (< l-count last-local) (< (- last-local l-count) 4))
      (list (- 251 (- last-local l-count)) (u2 offset))

      ;; append frame
      (and (= s-count 0) (< last-local l-count) (< (- l-count last-local) 4))
      (list (+ 251 (- l-count last-local))
            (u2 offset)
            (repeat obj (- l-count last-local)))

      ;; full frame
      t (list 0xFF (u2 offset)
              (if (= l-count 0)
                (list (u2 0))
                (list (u2 l-count) (repeat obj l-count)))
              (if (= s-count 0)
                (list (u2 0))
                (list (u2 s-count) (repeat obj s-count)))))))

(defun sframe-resolve0 (poff ploc acc lstat)
  (progn ;(print poff ploc acc lstat)
  (let (item (first lstat)
        offset (first item)
        stacks (second item)
        locals (third item))
    (if (nil? lstat)
      (reverse acc)
      (sframe-resolve0
        offset
        locals
        (cons (sframe1 (dec (- offset poff)) ploc locals stacks) acc)
        (rest lstat))))))

(defun sframe-resolve (argc offsets stack-i local-i)
  (->> (keyvals offsets)
       (msort (lambda (a b) (< (second a) (second b))))
       ;(print-iden)
       (map (lambda (x)
              (list (second x)
                    (get stack-i (first x) nil)
                    (get local-i (first x) nil))))
       (filter (lambda (x) (second x)))
       (sframe-resolve0 (- 1) argc nil)))

(defun arg-count (args)
  (if (index :rest args)
    (inc (index :rest args))
    (length args)))
