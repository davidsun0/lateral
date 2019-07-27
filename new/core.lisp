(defmacro defun (name args expr)
  (list (quote def) 
        name
        (list (quote fn) args expr)))

(defun not (p)
  (if p nil t))

(print (not nil))

(defun inc (n) (+ n 1))

; (print inc)
; (print (inc 100))

; (defun print-header ()
;  (progn
;    (pprint "#include <stdlib.h>")
;    (pprint "#include <stdio.h>")
;    (pprint "int main() {")))

; (defun print-footer ()
;  (pprint "}"))

;; (defun compile-print (obj)
;;   (cond
;;     ((list? obj)
;;      (join (map string obj))
;;     (t (printf "printf(\"%s\"\n);" (string obj))
