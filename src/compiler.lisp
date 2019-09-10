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

;; un-Matryoshka dolls a lisp expression
; ast:      abstract syntax tree to be destructured
; result:   accumulator for modified ast
; tree:     list of use-define expressions
; num:      working temp variable number for use-define chain
(defun destr (ast result tree num)
  (if (nil? ast)
    ; done with current list, add (marker, modified-ast) to list
    (cons (list (keyword (itoa num)) (reverse result)) tree)
    ; iterate across the current list
    (if (list? ast)
      ; when the ast contains an inner list...
      (if (list? (car ast))
              ; recursively destructure inner list
        (let (inner (destr (car ast) nil nil num)
              ; convert largest marker from result to integer
              nnum  (atoi (string (car (car inner)))))
          ; continue to next object in current list
          (destr (cdr ast)
                 ; insert largest marker into modified-ast
                 (cons (car (car inner)) result)
                 ; append destructured inner list to total tree
                 (concat inner tree)
                 ; increment marker number for next call
                 (inc nnum)))
        ; if ast does not contain inner list, continue traversing ast
        (destr (cdr ast) (cons (car ast) result) tree num)))))

(defun destr2 (ast)
  (reverse (destr ast nil nil 0)))

(defun to-ccode0 (ir acc)
  (if ir
    (let (item (car ir)
          term (if (keyword? item)
                 (str-cat "temp_" (string item))
                 (string item)))
      (if (cdr ir)
        (to-ccode0 (cdr ir) (cons ", " (cons term acc)))
        (cons term acc)))
    acc))

(defun to-ccode (var ir)
  (apply str-cat
    (reverse
      (concat (list ");")
        (to-ccode0 ir
                   (list " = funcall("
                         (string var)
                         "Object *temp_"))))))

; (print (to-ccode (quote (a b c d e))))

(defun use-define0 (astlist acc)
  (if astlist
    (let (term (car astlist)
          var  (car term)
          expr (cadr term))
      (use-define0 (cdr astlist)
                  (cons
                    (to-ccode var expr)
                    acc)))
    acc))

(defun use-define (astlist)
  (reverse!
    (cons
      (str-cat "return temp_"
               (string (car (last astlist)))
               ";")
      (use-define0 astlist nil))))

; (print (destr2 (quote (a 1))))
; (print (destr2 (quote (a (b 1)))))
; (print (destr2 (quote (a (b (c 1))))))
; (map print (use-define (destr2 (quote (a (b 1) (c 1))))))
(def distance-code
     (quote (sqrt (+ (sq (- x1 x2))
                     (sq (- y1 y2))))))
(map print (use-define (destr2 distance-code)))
