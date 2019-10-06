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

(defun or-deflate (expr acc)
  (print "implement or"))

(defun and-deflate (expr acc)
  (print "implement and"))

(defun cond-deflate0 (name expr acc tail? test-lab endlab)
  (if expr
    (let (test   (ir0 name (car expr) nil nil)
          ;TODO: check if branch is tail recursive
          branch (ir0 name (second expr) nil tail?)
          tail-recur? (equal? (first (last branch)) :tail-recur)
          _ (print "branch last")
          _ (print (last branch))
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
  (cond-deflate0 name expr nil tail? nil (uniquesym "lab_e")))

;;; first step in code processing
;;; turns a tree of lisp code into a stack-based intermediate representation
(defun ir0 (name ast acc tail?)
  (if ast
    (if (list? ast)
      (cond
        ;; 'if' special form
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

        ;; 'progn' special form
        (equal? (car ast) (quote progn))
        (progn-deflate name (cdr ast) nil tail?)

        (equal? (car ast) (quote cond))
        (progn
          (print ">>>")
          (cond-deflate name (cdr ast) nil tail?))
          ;(print "<<<"))

        (equal? (car ast) (quote or))
        (or-deflate expr nil)

        (equal? (car ast) (quote and))
        (or-deflate expr nil)
        
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
          ; TODO function lookup
          (equal? :funcall (car expr))
          ; name (nth 1 expr)
          ; argc (nth 3 expr)
          ; check if call is recursive?
          expr

          (not (equal? :push (car expr)))
          expr

          ; nil object
          (equal? (nth 1 expr) (quote nil)) (list :push :nil)
          ; true object
          (equal? (nth 1 expr) (quote t)) (list :push :true)

          ; literal constants
          (int? (nth 1 expr))    (list :push :int-const (nth 1 expr))
          (char? (nth 1 expr))   (list :push :char-const (nth 1 expr))
          (string? (nth 1 expr)) (list :push :str-const (nth 1 expr))

          ; otherwise lookup symbol in envir
          (symbol? (nth 1 expr))
          (if (index (nth 1 expr) arglist)
            (list :push :arg-num (index (nth 1 expr) arglist))
            (list :push :envir-sym (nth 1 expr)) acc)

          t expr)
        acc)))
    acc))

(defun resolve-syms (ir-list arglist)
  (reverse! (resolve-syms0 ir-list arglist nil)))
