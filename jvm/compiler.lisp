;;; reduces a tree to a list of lists
(defun semi-flatten0 (in acc)
  (if in
    (if (and (list? (car in)) (list? (car (car in))))
      (semi-flatten0 (cdr in)
                     (concat (car in) acc))
      (semi-flatten0 (cdr in) (cons (car in) acc)))
    (reverse! acc)))

(defun semi-flatten (in)
  (semi-flatten0 in nil))

;;; gensym for variables
(def uvar 0)
(defun uniquesym (prefix)
  (let (varc uvar)
    (progn
      (def uvar (inc varc))
      (keyword (str-cat prefix (string varc))))))

(defun progn-deflate (name expr acc tail?)
  (if expr
    (if (cdr expr)
      (progn-deflate name (cdr expr)
        (concat acc (append (ir0 name (car expr) nil nil) (list :pop))) tail?)
      (progn-deflate name (cdr expr)
        (concat acc (ir0 name (car expr) nil tail?)) tail?))
    acc))

(defun or-deflate (name end-lab expr acc)
  (if expr
    (or-deflate name end-lab
      (cdr expr)
                (concat acc (concat (ir0 name (car expr) nil nil)
                                    (list
                                      (list :dup)
                                      (list :jump-not-nil end-lab)
                                      (list :pop)
                                    ))))
    (concat acc (list (list :push :nil)
                      (list :label end-lab)))
    ))

(defun and-deflate (name false-lab expr acc)
  (if expr
    (and-deflate name false-lab
      (cdr expr)
                 (concat acc (append (ir0 name (car expr) nil nil)
                                     (list :jump-if-nil false-lab))))
    (let (end-lab (uniquesym "lab_e"))
    (concat acc (list (list :push :true)
                      (list :goto end-lab)
                      (list :label false-lab)
                      (list :push :nil)
                      (list :label end-lab)
                      )))
  ))

(defun cond-deflate0 (name expr acc tail? test-lab endlab)
  (if expr
    (let (test   (ir0 name (car expr) nil nil)
          branch (ir0 name (second expr) nil tail?)
          tail-recur? (equal? (first (last branch)) :tail-recur)
          ;_ (print test)
          ;_ (print branch)
          ;_ (pprint "")
          ;_ (print "branch last")
          ;_ (print (last branch))
          next-lab (uniquesym "lab_")
          irseg (concat test (cons (list :jump-if-nil next-lab)
                    (if tail-recur?
                      branch
                    (append branch (list :goto endlab)))
                    ))
          irseg (if test-lab (cons (list :label test-lab) irseg) irseg)
          )
    (progn
      ;(print "test:") (print test)
      ;(print "expr:") (print branch)
      ;(print "bytecode:")
      ;(print irseg)
  (cond-deflate0
    name (cdr (cdr expr))
    (concat acc irseg)
  tail? next-lab endlab)))
  (concat acc (list (list :label test-lab)
                (list :push :nil)
                    (list :label endlab)))
  ))

(defun cond-deflate (name expr acc tail?)
  (progn ;(print "cond expr") (print expr) (print "===")
  (cond-deflate0 name expr nil tail? nil (uniquesym "lab_e"))))

(defun let-deflate0 (bind-list acc)
  (if bind-list
  (let-deflate0 (cdr (cdr bind-list))
                (concat acc
  (list (ir0 nil (second bind-list) nil nil)
        (list :let-set (car bind-list)))))
  (reverse acc)))

(defun let-deflate (name expr acc tail?)
  (list 
    (list :let-bind)
    (semi-flatten (let-deflate0 (car expr) nil))
    (list :let-body)
    (reverse (ir0 name (second expr) nil tail?))
    (list :let-pop)))

;;; first step in code processing
;;; turns a tree of lisp code into a stack-based intermediate representation
(defun ir0 (name ast acc tail?)
  (if ast
    (if (list? ast)
      (cond
        ;; if
        (equal? (car ast) (quote if))
        (let (else-lab (uniquesym "lab_")
              end-lab  (uniquesym "lab_e")
              t-branch (reverse (ir0 name (nth 2 ast) nil tail?))
              true-recur? (equal? (car (car t-branch)) :tail-recur))
              ; _ (print true-recur?)
              ; _ (progn (print "[") (print t-branch) (print "]")))
        (semi-flatten
          (list
            (reverse (ir0 name (nth 1 ast) nil nil)) ; condition
            (list :jump-if-nil else-lab)
            t-branch ; true branch
            (if (not true-recur?) (list :goto end-lab))
            (list :label else-lab)
            (if (= (length ast) 4)
              (reverse (ir0 name (nth 3 ast) nil tail?)) ; false branch
              (list :push :nil))
            (if (not true-recur?) (list :label end-lab))
            )))

        ;; progn
        (equal? (car ast) (quote progn))
        (progn-deflate name (cdr ast) nil tail?)

        ;; cond
        (equal? (car ast) (quote cond))
        (cond-deflate name (cdr ast) nil tail?)

        ;; let
        (equal? (car ast) (quote let))
        (semi-flatten (let-deflate name (cdr ast) nil tail?))

        ;; or
        (equal? (car ast) (quote or))
        (or-deflate name (uniquesym "lab_") (cdr ast) nil)
        ;(print "implement or ir")

        ;; and
        (equal? (car ast) (quote and))
        (and-deflate name (uniquesym "lab_") (cdr ast) nil)
        ;(print "implement and ir")

        ;; quote
        (equal? (car ast) (quote quote))
        (if (nth 1 ast)
          (list (list :push :symbol (nth 1 ast)))
          (print "don't know how to quote list"))

        (and tail? (equal? name (car ast)))
        (concat
          (ir1 (cdr ast) nil)
          (list (list :tail-recur :argc (dec (length ast)))))

        t
        (concat
          (ir1 (cdr ast) nil)
          (list (list :funcall (car ast)
                      :argc (dec (length ast))))))
      (list (list :push ast)))
    acc))

;; iterates along list, resolving nested lists with ir0
(defun ir1 (ast acc)
  (if ast
    (if (list? (car ast))
      (ir1 (cdr ast)
           (concat acc (ir0 name (car ast) nil nil)))
      (ir1 (cdr ast)
           (append acc (list :push (car ast)))))
    acc))

(defun ir (name ast)
  (ir0 name ast nil t))

;;; looks up argument and environment variables
;;; finds constants
;; stack ir -> stack ir
(defun resolve-syms0 (ir-list arglist acc)
  (if ir-list
    (let (expr (car ir-list))
      (resolve-syms0
        (cdr ir-list)
        arglist
        (cons
        (cond
          ;function lookup
          ;(equal? :funcall (car expr))
          ;expr

          (not (equal? :push (car expr)))
          expr

          ; push nil
          (equal? (nth 1 expr) (quote nil)) (list :push :nil)
          ; push true
          (equal? (nth 1 expr) (quote t)) (list :push :true)

          ; push literal constants
          (int? (nth 1 expr))    (list :push :int-const (nth 1 expr))
          (char? (nth 1 expr))   (list :push :char-const (nth 1 expr))
          (string? (nth 1 expr)) (list :push :str-const (nth 1 expr))
          ;(symbol? (nth 1 expr)) (list :push :symbol (nth 1 expr))
          (equal? (nth 1 expr) :symbol) expr

          ; otherwise lookup symbol in envir
          (symbol? (nth 1 expr))
          (if (index (nth 1 expr) arglist)
            (list :push :arg-num (index (nth 1 expr) arglist))
            (list :push :envir-sym (nth 1 expr)))

          t expr)
        acc)))
    acc))

(defun resolve-syms1 (ir-list arglist letlist acc)
  (if ir-list
    (let (expr (car ir-list))
          ;_ (print "resolve-syms1"))
      (cond
        (equal? (car expr) :let-bind);(resolve-syms2 (cdr ir-list) arglist acc)
        (progn ;(print "let-bind???")
        (let (ir-and-acc (resolve-syms1 (cdr ir-list)
                                        (concat arglist letlist)
                                        nil
                                        (cons (list :let-push) acc))
              ;_ (print "let-bind================")
              ;_ (print (first ir-and-acc)))
              )
          (resolve-syms1 (first ir-and-acc) arglist nil (second ir-and-acc))))

        (equal? (car expr) :let-set)
        (let (;_ (print "let-set")
              inlet? (index (nth 1 expr) letlist)
              letlist (if inlet? letlist (append letlist (nth 1 expr)))
              argidx (+ (length arglist) (if inlet? inlet? (dec (length letlist))))
              ;_ (print (nth 1 expr))
              ;_ (print letlist)
              ;_ (print argidx)
              )
            (resolve-syms1
              (cdr ir-list)
              arglist
              letlist
              (cons (list :local-count (uniquesym "lc_") (+ (length letlist) (length arglist)))
              (cons (list :store :arg-num argidx) acc))))

        (equal? (car expr) :let-body)
        ;(progn (print "let-body") (print arglist) (print letlist))
        (resolve-syms1 (cdr ir-list) arglist letlist acc)

        (equal? (car expr) :let-pop)
        (list (cdr ir-list) (cons (list :let-pop (uniquesym "lp_") (length arglist)) acc))
        ;(progn (print "===================="))

        t
        (resolve-syms1
          (cdr ir-list)
          arglist
          letlist
          (cons
            (cond
              (not (equal? :push (car expr))) expr
              (equal? (nth 1 expr) (quote nil)) (list :push :nil)
              (equal? (nth 1 expr) (quote t)) (list :push :true)
              (int? (nth 1 expr)) (list :push :int-const (nth 1 expr))
              (char? (nth 1 expr)) (list :push :char-const (nth 1 expr))
              (string? (nth 1 expr)) (list :push :str-const (nth 1 expr))

              (symbol? (nth 1 expr))
              (let (letidx (index (nth 1 expr) letlist)
                    idx (if letidx
                          (+ (length arglist) letidx)
                          (index (nth 1 expr) arglist)))
              (if ;(index (nth 1 expr) arglist) ;TODO change to last index
                idx
                (list :push :arg-num idx)
                (list :push :envir-sym (nth 1 expr))))

              t expr)
            acc))))
    (list :asdfasdf acc)))

(defun resolve-syms (ir-list arglist)
  (let (a (resolve-syms1 ir-list arglist nil nil)
        ;_ (print (first a))
        ;_ (print (second a))
        )
  (reverse! (second a))))
