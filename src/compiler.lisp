;; it took me three whole days to write ssa-base and ssa-main
(defun ssa-base (tree)
  (if (list? tree)
    ;; add tree to its list of children
    (concat (ssa-main tree nil) (list tree))
    ;; return wrapped list to simplify concatenation
    (list tree)))

(defun ssa-main (tree acc)
  ;; apply ssa-base over list and collect results
  (if tree
    (ssa-main (cdr tree)
              (concat (ssa-base (car tree)) acc))
    acc))

(def my-ast (reverse (filter list? (ssa-base (expr nth)))))
; (map print my-ast)

(defun destr (ast result tree)
  (if (nil? ast)
    (cons (reverse result) tree)
    (if (list? ast)
      (if (list? (car ast))
        (destr (cdr ast) (cons :: result)
               (cons (destr (car ast) nil nil) tree))
        (destr (cdr ast) (cons (car ast) result) tree))
      ast)))

(print (destr (quote (a 1)) nil nil))
(print (destr (quote (a (b 1))) nil nil))
(print (destr (quote (a (b (c 1)))) nil nil))
(print (destr (quote (a (b 1) (c 1))) nil nil))
