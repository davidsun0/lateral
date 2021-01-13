(define (qq-expand x depth)
  (if (pair? x)
    (case (car x)
      ((quasiquote)
       `(cons 'quasiquote
              ,(qq-expand (cdr x) (+ depth 1))))
      ((unquote unquote-splicing)
       (cond ((> depth 0)
              `(cons ',(car x)
                     ,(qq-expand (cdr x) (- depth 1))))
             ((and (eq? 'unquote (car x))
                   (not (null? (cdr x)))
                   (null? (cddr x)))
              (cadr x))
             (else
               (error "Illegal"))))
      (else
        `(append ,(qq-expand-list (car x) depth)
                 ,(qq-expand (cdr x) depth))))
    `',x))

(define (qq-expand-list x depth)
  (if (pair? x)
    (case (car x)
      ((quasiquote)
       `(list (cons 'quasiquote
                    ,(qq-expand (cdr x) (+ depth 1)))))
      ((unquote unquote-splicing)
       (cond ((> depth 0)
              `(list (cons ',(car x)
                           ,(qq-expand (cdr x) (- depth 1)))))
             ((eq? 'unquote (car x))
              `(list . ,(cdr x)))
             (else
               `(append . ,(cdr x)))))
      (else
        `(list (append ,(qq-expand-list (car x) depth)
                       ,(qq-expand (cdr x) depth)))))
    `'(,x)))
