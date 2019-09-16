(def distance-code
     (quote (lambda (x1 y1 x2 y2)
              (sqrt (+ (sq (- x1 x2))
                       (sq (- y1 y2)))))))

;; gensym
(def uvar 0)
(defun uniquesym (prefix)
  (let (varc uvar)
    (progn
      (def uvar (inc varc))
      (keyword (str-cat prefix (string varc))))))

(defun uniquesym-reset ()
  (progn (def uvar 0)
         t))

;; converts lisp ast to sequential intermediate representation
;; ast:    abstract syntax tree to analyze
;; result: current line accumulator
;; tree:   overall ir accumulator
;; var:    keyword to assign value of ast to
(defun to-ir0 (ast result tree var)
  (if ast
    (if (list? ast)
      ; recursively convert nested lists
      (if (list? (car ast))
        (let (inner (to-ir0 (car ast) nil nil var))
          (to-ir0 (cdr ast)
                  ; replace nested expression with return variable
                  (cons (car (car inner)) result)
                  ; append inner ir to ir accumulator
                  (concat inner tree)
                  (uniquesym "var_")))
        (to-ir0 (cdr ast) (cons (car ast) result) tree var)))
    ; done processing list, return accumulator
    (cons (list var (reverse! result)) tree)))

(defun to-ir1 (ast)
  (let (ir-reverse (to-ir0 ast nil nil (uniquesym "var_"))
        result     (car (car ir-reverse)))
    (reverse! (cons (list :return result)
                    (map (lambda (x) (cons :assign x))
                         ir-reverse)))))

(defun ir0 (ast acc)
  (if ast
    (if (list? ast)
      (ir1 (cdr ast)
           (cons (list :funcall
                       (car ast)
                       :argc
                       (dec (length ast)))
                 acc))
      (list :push ast))
    acc))

(defun ir1 (ast acc)
  (if ast
    (if (list? (car ast))
      (ir1 (cdr ast)
           (concat (ir0 (car ast) nil)
                   acc))
      (ir1 (cdr ast)
           (cons (list :push (car ast))
                 acc)))
    acc))

(map print (ir0 (nth 2 distance-code) nil))
