(defun not (p)
  (if p nil t))

(defun nil? (p)
  (if p nil t))

(defun inc (n)
  (+ 1 n))

(defun dec (n)
  (- n 1))

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

(defun length0 (list acc)
  (if list
    (length0 (rest list) (inc acc))
    acc))

(defun length (list)
  (length0 list 0))

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
    (index0 needle (rest haystack) (inc acc))))

(defun index (needle haystack)
  (index0 needle haystack 0))

(defun append (in obj)
  (reverse (cons obj (reverse in))))

;(defun cons (:rest l)
;  (let (rl (reverse l))
;    (reverse0 (rest rl) (first rl))))

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

(defun get (hmap key :rest missing)
  (let (res (get0 hmap key))
    (cond
      (nil? missing) res
      (second res)   (first res)
      t              (first missing))))

(def *gensym-count* 0)
(defun gensym (:rest prefix)
  (symbol
    (string0 
      (list (if prefix (first prefix) "gsym")
            (def *gensym-count*
                 (inc *gensym-count*))))))

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
        (lambda? func) (lambda-apply func (rest eval-list) env)
        t (progn (print "error: ") (print func) (print "isn't function"))))))

(defun eval (ast)
  (apply0 ast (user-envir)))

(defun include (path)
  (eval (cons (quote progn) (read-all path))))

;(defun apply (fn args)
;  (->> args
;       (map (lambda (x) (list 'quote x)))
;       (cons fun)
;       (eval)))

(defun main ()
  (progn
    (pprint0 "user>> ")
    (print (apply0 (read (readline)) (user-envir)))
    (main)))

;; expands macros, part1
(defun ast-analysis0 (in)
  (if (list? in)
    (ast-analysis1 in nil)
    in))

;; expands macros, part2
(defun ast-analysis1 (in acc)
  (cond
    (nil? in) (reverse acc)

    (and (nil? acc) (contains? macros (first in)))
    (ast-analysis0 (macro-expand (get macros nil) in))

    t (ast-analysis1 (rest in) (cons (ast-analysis0 (first in)) acc))))

;; extracts lambdas
(defun lambda-extr0 (in lambdas)
  (if (list? in)
    (lambda-extr1 in nil nil)
    (list in nil)))

;; extracts lambdas, part2
(defun lambda-extr1 (in acc lambdas)
  (cond
    (nil? in) (cons (reverse acc) lambdas)
    (and (nil? acc) (equal? (quote quote) (first in))) (list in)

    (and (nil? acc) (equal? (quote lambda) (first in)))
    (let (l-name (gensym "lambda"))
      ;; TODO: handle nested lambdas
      (cons l-name (cons l-name (rest in)) lambdas))

    t
    (let (e-l (lambda-extr0 (first in) nil)
          expr (first e-l)
          lamb (second e-l))
      (lambda-extr1 (rest in)
                    (cons expr acc)
                    (if (nil? lamb)
                      lambdas
                      (cons lamb lambdas))))))

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
;`(defun ,fname (name args largs expr acc ,@(vars))
;   (if expr
;     (,fname name args largs
;             ,expr-mod
;             ,acc-mod
;             ,@(vars-mod))
;     (cons ,acc-add acc)))

(defun progn-deflate (name args largs expr acc)
  (if expr
    (progn-deflate
      name args largs
      (rest expr)
      (cons
        (list
          (if (rest expr)
            (list :pop)
            (cons nil nil))
          (ir0 (if (rest expr) nil name)
               args largs
               (first expr) nil))
         acc))
    acc))

(defun or-deflate (name args largs end-lab expr acc)
  (if expr
    (or-deflate name args largs end-lab
                (rest expr)
                (cons
                (list (list :pop)
                      (list :jump-not-nil end-lab)
                      (list :dup)
                      (ir0 nil args largs (first expr) nil))
                acc))
    (cons (list (if name
                  (list :return)
                  (cons nil nil))
                (list :label end-lab)
                (list :push :nil))
          acc)))

(defun and-deflate (name args largs false-lab expr acc)
  (if expr
    (and-deflate name args largs
                 false-lab
                 (rest expr)
                 (cons (list (if (rest expr)
                               (list :jump-if-nil false-lab)
                               (cons nil nil))
                             (ir0 nil args largs (first expr) nil))
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

(defun cond-deflate (name args largs expr test-lab end-lab acc)
  (if expr
    (let (test     (first expr)
          branch   (second expr)
          next-lab (gensym "cond-"))
      (cond-deflate name args largs
                    (rest (rest expr)) next-lab end-lab
                    (cons (list (if name
                                  (cons nil nil)
                                  (list :goto end-lab))
                                (ir0 name args largs branch nil)
                                (list :jump-if-nil next-lab)
                                (ir0 nil args largs test nil)
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
                      (append largs (first bind-list)))
          _ (print (first bind-list) largs-new))
      (let-deflate0 args
                    largs-new
                    (rest (rest bind-list))
                    (cons (list :store (+ (length args)
                                          (index (first bind-list) largs-new)))
                          (ir0 nil args largs (second bind-list) nil)
                          acc)))
    (list largs acc)))

(defun let-deflate (name args largs expr)
  (let (x (let-deflate0 args largs (first expr) nil)
        largs (first x)
        bind-ir (second x))
  (list
    (list :let-pop (gensym "letp") (length args))
    (ir0 name args largs (second expr) nil)
    (list :local-count (gensym "letc") (+ (length args) (length largs)))
    bind-ir)))

;; iterates along list, resolving nested lists with ir0
(defun ir1 (args largs ast acc)
  (cond
    (nil? ast) acc

    (list? (first ast))
    (ir1 args largs (rest ast)
         (concat (ir0 nil args largs (first ast) nil) acc))

    t
    (ir1 args largs (rest ast)
         (cons
           (if (symbol? (first ast))
             (cond
               (equal? (first ast) (quote nil)) (list :push :nil)
               (equal? (first ast) (quote t)) (list :push :true)

               (index (first ast) largs)
               (list :push :arg-num (+ (length args) (index (first ast) largs)))

               (index (first ast) args)
               (list :push :arg-num (index (first ast) args))

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
               (equal? (first ast) (quote nil)) :nil
               t :unknown)
             (first ast)))
           acc))))

;; first step in code processing
;; turns a tree of lisp code into a stack-based intermediate representation
;; name doubles as a flag for if the expression is in the tail position
(defun ir0 (name args largs ast acc)
  (cond
    (nil? ast) (reverse acc)

    (not (list? ast))
    (list (if name (list :return) (list nil))
          (ir1 args largs (list ast) nil))

    (equal? (first ast) (quote if))
    (let (false-label (gensym "if-f")
          end-label (gensym "if-e"))
      (list
        (if (not name) (list :label end-label) (cons nil nil))
        (if (= (length ast) 4)
          ;; has an else branch
          (ir0 name args largs (nth 3 ast) nil)
          ;; no else branch
          (list (if name
                  (list :return)
                  (cons nil nil))
                (list :push :nil)))
        (list :label false-label)
        (if (nil? name)
          (list :goto end-label)
          (list nil))
        (ir0 name args largs (third ast) nil)
        (list :jump-if-nil false-label)
        (ir0 nil args largs (second ast) nil)))

    (equal? (first ast) (quote and))
    (and-deflate name args largs (gensym "and-f") (rest ast) nil)

    (equal? (first ast) (quote or))
    (or-deflate name args largs (gensym "or-e") (rest ast) nil)

    (equal? (first ast) (quote cond))
    (cond-deflate name args largs (rest ast) nil (gensym "cond-e") nil)

    (equal? (first ast) (quote progn))
    (progn-deflate name args largs (rest ast) nil)

    (equal? (first ast) (quote let))
    (let-deflate name (concat args largs) nil (rest ast))

    (equal? (first ast) (quote quote))
    ;(list (list :push :quote (second ast)))
    (list (if name (list :return) (list nil))
      (list :push :symbol (second ast)))

    (equal? (first ast) (quote lambda))
    (list (list :lambda ast))

    ;; TODO: check arg is symbol
    (equal? (first ast) (quote def))
    (list (if name (list :return) (list nil))
          (list :funcall :envir-set :argc 2)
          (ir0 nil args largs (third ast) nil)
          (list :push :symbol (second ast)))

    (equal? (first ast) name)
    (cons (list :tail-recur :argc (dec (length ast)))
          (ir1 args largs (rest ast) nil))

    name (cons (list :return) (ir0 nil args largs ast acc))

    (or (index (first ast) args)
        (index (first ast) largs))
    (cons (list :dynamcall :argc (length ast))
          (ir1 args largs ast nil))

    t
    (cons (list :funcall (first ast) :argc (dec (length ast)))
          (ir1 args largs (rest ast) nil))))
