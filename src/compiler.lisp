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

;;; first step in code processing
;;; turns a tree of lisp code into a stack-based intermediate representation
(defun ir0 (ast acc)
  (if ast
    (if (list? ast)
      (cond
        ;; 'if' special expression
        (equal? (car ast) (quote if))
        (let (else-lab (uniquesym "lab_")
              end-lab  (uniquesym "lab_e"))
        (semi-flatten
          (list
            (reverse (ir0 (nth 1 ast) nil)) ; condition
            (list :jump-if-nil else-lab)
            (reverse (ir0 (nth 2 ast) nil)) ; true branch
            (list :goto end-lab)
            (list :label else-lab)
            (if (= (length ast) 4)
              (reverse (ir0 (nth 3 ast) nil)) ; false branch
              (list :push :nil))
            (list :label end-lab))))

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
           (concat acc (ir0 (car ast) nil)))
      (ir1 (cdr ast)
           (append acc (list :push (car ast)))))
    acc))

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

          ; nil literal
          (equal? (nth 1 expr) (quote nil))
          (list :push :nil)

          ; true literal
          (equal? (nth 1 expr) (quote t))
          (list :push :true)

          ; integer constant
          (int? (nth 1 expr))
          (list :push :int-const (nth 1 expr))

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
