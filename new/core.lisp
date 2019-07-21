(def quote (macro (x) x))

; (def name (macro (args) expr))
; (defmacro name args expr)

(quote (name args expr))

(def defmacro (macro (name args expr) (list (quote def) name (list (quote macro) args expr))))
(debug defmacro)

(defmacro a b c)
(def x 123)
(defmacro quote (x) x)

(defmacro defun (name args expr) (list (quote def) name (list (quote fn) args expr)))

(defun inc (x) (+ x 1))

(inc 123)
