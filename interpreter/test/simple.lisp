;; Lateral Unit Tests
;; To add a test: type the textual input followed by a new line, a semicolon,
;; and then the expected output.
;; Tests should be split with an empty line.

;; Arithmetic
12345
; 12345

(+ 1 1)
; 2

(+ 1 2 3 4 5)
; 15

(+ 1
   (+ 1 1))
; 3

;; Function calls
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

;; Global Environment
(def x (+ 100 200))
; 300

x
; 300

(def x 31415)
; 31415

x
; 31415

;; Special forms
(def a "text")
; "text"

a
; "text"

(quote a)
; a
