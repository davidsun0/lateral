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

(defun not (p)
  (if p nil t))

(defun nil? (p)
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
    (pprint "user>> ")
    (print (apply (read (readline)) (user-envir)))
    (main)))
