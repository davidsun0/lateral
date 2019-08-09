; it took me three whole days to write ssa-base and ssa-main
(defun ssa-base (tree)
  (if (list? tree)
    (concat (ssa-main tree nil) (list tree))
    (list tree)))

(defun ssa-main (tree acc)
  (if tree
    (ssa-main (cdr tree)
              (concat (ssa-base (car tree)) acc))
    acc))

(defun ssa-base2 (id tree)
  (if (list? tree)
    (let (result (ssa-main2 (inc id) tree (quote ()))
          syms   (car result)
          new-id (inc (cadr result)))
      (cons (list tree new-id)
            syms
            ))
    (list (list tree (inc id)))))

(defun ssa-main2 (id tree acc)
  (if tree
    (let (result (ssa-base2 id (car tree))
          new-id (inc (car result)))
      (progn
        (print ">")
        (print (cdar result))
      (ssa-main2 new-id 
               (cdr tree)
               (concat result
                       acc))))
    (list acc id)))

(defun temp (arg)
  (let (new (inc arg))
    new))

(def test-tree (quote ((a) b (c) d)))
; (print (ssa-base (quote (1))))
; (print (ssa-base (quote (1 2 3))))
; (print "===")
; (map print (ssa-base (quote (1 (2) 3))))
; (print "===")
; (map print (ssa-base test-tree))
; (print "===")
; (print (ssa-base2 0 (quote a)))
;(def my-ast (ssa-base2 0 (quote (a b c (d e)))))
(def my-ast (ssa-base (expr max0)))
(map print my-ast)
; (map print (ssa-base2 0 (quote (a b c (d e)))))
; (map print (ssa-base2 0 (expr max0)))
