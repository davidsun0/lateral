;; ARITHMETIC
12345
; 12345

(+ 1 1)
; 2

(+ 1 2 3 4 5)
; 15

(+ 1
   (+ 1 1))
; 3

;; FUNCTIONS
((lambda () 321))
; 321

((lambda (x)
   (+ x x x))
 333)
; 999

((lambda (x)
  ((lambda (y) (+ y 1)) x))
  10)
; 11

;; ENVIR
(def x (+ 100 200))
; 300

x
; 300

(def x 31415)
; 31415

x
; 31415

;;SPECIAL
(def a "text")
; "text"

a
; "text"

(quote a)
; a
