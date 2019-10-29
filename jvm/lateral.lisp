(defun not (p)
  (if p nil t))

(defun second (list)
  (first (rest list)))

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
    (reverse0 (rest in) (cons (first in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

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

(defun make-token (in-string start end acc)
  (if (equal? start end)
    acc
    (cons (substr in-string start end) acc)))

(print "tokenize")
(defun tokenize (in-string tok-start idx acc state)
  (if (char-at in-string idx)
    (cond
      ; detect end of comment
      (and (equal? state "comment") (equal? (char-at in-string idx) "\n"))
      (tokenize in-string (inc idx) (inc idx) acc nil)

      ; ignore all characters in comment
      (equal? state "comment")
      (tokenize in-string (inc idx) (inc idx) acc state)

      ; detect start of comment
      (and (equal? (char-at in-string idx) ";")
           (not (equal? state "quote")))
      (tokenize in-string (inc idx) (inc idx) acc "comment")

      ; end of comment
      (and (equal? state "quote")
           (equal? (char-at in-string idx) "\""))
      (tokenize in-string (inc idx) (inc idx)
                (make-token in-string tok-start (inc idx) acc) nil)
    
      ; continue quote
      (equal? state "quote")
      (if (and (equal? (char-at in-string idx) "\\")
               (or (equal? (char-at in-string (inc idx)) "n")
                   (equal? (char-at in-string (inc idx)) "\\")
                   (equal? (char-at in-string (inc idx)) "\"")))
        (tokenize in-string tok-start (inc (inc idx)) acc "quote")
        (tokenize in-string tok-start (inc idx) acc "quote"))

      ; start of comment
      (equal? (char-at in-string idx) "\"")
      (tokenize in-string tok-start (inc idx) acc "quote")

      ; ignore whitespace
      (whitespace? (char-at in-string idx))
      (tokenize in-string (inc idx) (inc idx)
                (make-token in-string tok-start idx acc) state)

      ; left paren
      (equal? (char-at in-string idx) "(")
      (tokenize in-string (inc idx) (inc idx)
                (cons "(" (make-token in-string tok-start idx acc)) state)
     
      ; right paren
      (equal? (char-at in-string idx) ")")
      (tokenize in-string (inc idx) (inc idx)
                (cons ")" (make-token in-string tok-start idx acc)) state)
      
      t (tokenize in-string tok-start (inc idx) acc state))
    (reverse acc)))

;; hacky way of forward defining functions for mutual recursion
(insert-method "read-list" "Lateral" "read-list" 2)

(defun read-form (token-list)
  (if token-list
    (if (equal? (first token-list) "(")
      (read-list (rest token-list) nil)
      (list (read-atom (first token-list))
            (rest token-list)))))

(defun readForm (token-list)
  (read-form token-list))

(defun read-list (token-list acc)
  (if (equal? (first token-list) ")")
    (list (reverse acc) (rest token-list))
    (let (res-and-tokens (read-form token-list)
          result (first res-and-tokens)
          new-tokens-list (second res-and-tokens))
      (if res-and-tokens
        (read-list new-tokens-list
                   (cons result acc))
                   (list acc nil)))))

(defun read ()
  (let (_ (pprint "user> ")
        form (read-form (tokenize (readline) 0 0 nil nil)))
    (first form)))

(insert-method "apply" "Lateral" "apply" 2)

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
    (let (eval-list (eval ast env nil)
          func (first eval-list))
      (cond
        (native-fn? func) (native-invoke func (rest eval-list))
        (lambda? func) (lambda-apply func (rest eval-list) env)
        t (progn (print "error: ") (print func) (print "isn't function"))))))

(defun main ()
  (progn
    (print (apply (read) (user-envir)))
    (main)))
