(def distance-code
     (quote (lambda (x1 y1 x2 y2)
              (sqrt (+ (sq (- x1 x2))
                       (sq (- y1 y2)))))))

(defun semi-flatten0 (in acc)
  (if in
    (if (and (list? (car in)) (list? (car (car in))))
      (semi-flatten0 (cdr in)
                     (concat (car in) acc))
      (semi-flatten0 (cdr in) (cons (car in) acc)))
    acc))

(defun semi-flatten (in)
  (reverse! (semi-flatten0 in nil)))

(def uvar 0)
(defun uniquesym (prefix)
  (let (varc uvar)
    (progn
      (def uvar (inc varc))
      (keyword (str-cat prefix (string varc))))))

(defun ir0 (ast acc)
  (if ast
    (if (list? ast)
      (cond
        (equal? (car ast) (quote if))
        (let (else-lab (uniquesym "lab_")
              end-lab  (uniquesym "lab_e"))
        (semi-flatten
          (list
            (reverse (ir0 (nth 1 ast) nil)) ; condition
            (list :jump-if-nil else-lab)
            (reverse (ir0 (nth 2 ast) nil)) ; true branch
            (list :goto end-lab)
            (list :label else-lab)
            (if (= (length ast) 4)
              (reverse (ir0 (nth 3 ast) nil)) ; false branch
              (list :push :nil))
            (list :label end-lab))))

        t
        (concat
        (ir1 (cdr ast) nil)
             (list (list :funcall (car ast)
                         :argc (dec (length ast))))))
      (list (list :push ast)))
    acc))

(defun ir1 (ast acc)
  (if ast
    (if (list? (car ast))
      (ir1 (cdr ast)
           (concat acc (ir0 (car ast) nil)
                   ))
      (ir1 (cdr ast)
           (append acc (list :push (car ast)))))
    acc))

(defun max-stack0 (in max-c curr-c s-stack)
  (if in
    (let (expr (car in)
          max-c (if (< max-c curr-c) curr-c max-c))
      (cond
        ; push increases stack by 1
        (equal? (car expr) :push)
        (max-stack0 (cdr in) max-c (inc curr-c) s-stack)

        ; funcall pops argc, pushes 1 result
        (equal? (car expr) :funcall)
        (max-stack0 (cdr in)
                    max-c
                    (inc (- curr-c (nth 3 expr)))
                    s-stack)

        ; jump-if-nil consumes 1 for test
        (equal? (car expr) :jump-if-nil)
        ; add :label, (dec curr-c) to working stack (jump-if-nil's pop)
        ; curr-c will be restored at corresponding label
        (max-stack0 (cdr in) max-c (dec curr-c)
                    (cons (nth 1 expr)
                          (cons (dec curr-c) s-stack)))

        ; restore curr-c from s-stack
        ; working curr-c can be ignored because any branch of the if statement
        ; must push one and only one object to the stack
        (and (equal? (car expr) :label)
             (equal? (car s-stack) (nth 1 expr)))
        (max-stack0 (cdr in) max-c (nth 1 s-stack) (cdr (cdr s-stack)))

        t (max-stack0 (cdr in) max-c curr-c s-stack)
        ))
    max-c))

(defun resolve-syms0 (ir-list arglist acc)
  (if ir-list
    (let (expr (car ir-list))
      (resolve-syms0
        (cdr ir-list)
        arglist
        (cons
        (cond
          ; TODO function lookup
          (equal? :funcall (car expr))
          ; name (nth 1 expr)
          ; argc (nth 3 expr)
          ; check if call is recursive?
          expr

          (not (equal? :push (car expr)))
          expr

          ; nil literal
          (equal? (nth 1 expr) (quote nil))
          (list :push :nil)

          ; true literal
          (equal? (nth 1 expr) (quote t))
          (list :push :true)

          ; integer constant
          (equal? (type (nth 1 expr)) :int)
          (list :push :int-const (nth 1 expr))

          ; otherwise lookup symbol in envir
          (equal? (type (nth 1 expr)) :symbol)
          (if (index (nth 1 expr) arglist)
            (list :push :arg-num (index (nth 1 expr) arglist))
            (list :push :envir-sym (nth 1 expr)) acc)
          
          t expr)
        acc)))
    acc))

(defun resolve-syms (ir-list arglist)
  (reverse! (resolve-syms0 ir-list arglist nil)))

(defun ir-to-jvm (expr)
  (let (cmd (car expr))
    (cond
      (equal? cmd :return)
      (list :areturn)

      (equal? cmd :push)
      (cond
        (equal? (nth 1 expr) :arg-num)
        (list :aload (nth 2 expr)))
      )))

; (print (max-stack0 (ir0 (nth 2 distance-code) nil) 0 0 nil))
; (map print (ir0 (nth 2 distance-code) nil))
(def ast (quote (if (= (+ nil 1) 2) (+ 1 2 3) (- 4 5 6))))
; (map print (ir0 (quote (if (= (+ 1 1) 2) (+ 1 2 3) (- 4 5 6))) nil))
;(map print (ir0 ast nil))
; (print (ir0 (quote x) nil))
;(print (max-stack0 (ir0 ast nil) 0 0 nil))
; (map print (reverse (resolve-syms0 (ir0 (quote x) nil)
;                       (quote (x)) nil)))

(defun compile (name args expr)
  (let (ir-list (append (ir0 expr nil) (list :return))
        stack-size (max-stack0 ir-list 0 0 nil)
        ir-list (map ir-to-jvm (resolve-syms ir-list args)))
    (progn
      (map print ir-list)
      (print stack-size))))

(compile "main" (quote (a)) (quote a))


