(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote fn) args expr)))

(defun print-header ()
  (progn
    (pprint "#include <stdlib.h>")
    (pprint "#include <stdio.h>")
    (pprint "int main() {")))

(defun print-footer ()
  (pprint "}"))

(defun compile-print (obj)
  (cond
    ((list? obj) ())
    (t (printf "printf(\"%s\"\n);" (string obj))
