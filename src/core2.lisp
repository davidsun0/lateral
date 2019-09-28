(include "jvmclass.lisp")

(def funlist nil)

(defun prepend (elem listy)
  (cons (list elem) listy))

(defmacro defun (name args expr)
; `(def funlist (cons (compile1 ,name ,args ,expr) funlist))
  (list
    (quote def)
    (quote funlist)
    (list
      (quote prepend)
      (list (quote compile1)
            (list (quote string) (list (quote quote) name))
            (list (quote quote) args)
            (list (quote quote) expr))
      (quote funlist))))

(defun identity (x)
  x)

(defun not (p)
  (if p nil t))

(defun length (in acc)
  (if in
    (length (cdr in) (inc acc))
    acc))

(write-bytes
  "Lateral.class"
  (flatten
    (class-headers
      "Lateral"
      "java/lang/Object"
      funlist)))
