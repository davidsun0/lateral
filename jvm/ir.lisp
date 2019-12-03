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
                      (append largs (first bind-list))))
      (let-deflate0 args
                    largs-new
                    (rest (rest bind-list))
                    ;; store expression into local var
                    (cons (list :store (+ (length args)
                                          (index (first bind-list) largs-new)))
                          ;; ir0 uses old largs because current bind var hasn't
                          ;; been assigned yet
                          ;; ir for expression
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

(defun ir (name args ast)
  ; remove '(nil) generated in ir0
  (filter first
    (reverse (semi-flatten (ir0 name args nil ast nil)))))
