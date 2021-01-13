(defun addn (n)
  (lambda (x) (+ n x)))

(defun print1 (plist)
  (if plist
    (progn (print0 (first plist))
           (pprint0 " ")
           (print1 (rest plist)))
    (pprint0 "\n")))

(defun print (:rest x)
  (print1 x))

(defun main ()
  (let (f (addn 5))
    (progn
      (print addn)
      (print 1 2 3 4 5)
      (print (map (lambda (x) (+ x 100))
                (list 1 2 3 4 5)))
      (print (f 100))
      (print (f 0))
      (print (map f (list 10 9 8 7 6))))))

(defun test ()
  (->> '(int? (first x) (a (b c)))
       (closure-flatten1 '(x) nil)
       (semi-flatten)
       ;(reverse)
       (map print)
       ))

(defun closure-flatten0 (args locals ast acc)
  (cond
    (nil? ast) (reverse acc)
    (list? ast) (closure-flatten0
                  args
                  locals
                  (rest ast)
                  (cons (closure-flatten1 args locals (first ast)) acc))
    (index ast args) (list :push :symbol ast)
    (index ast locals) (list :push :arg-num (index ast locals))
    (int? ast) (list :push :int-const ast)
    (string? ast) (list :push :str-const ast)
    (keyword? ast) (list :push :keyword ast)

    (symbol? ast)
    (list
      (list :push :symbol 'quote)
      (list :push :symbol ast)
      (list :funcall 'envir-get :argc 1)
      (list :funcall 'list :argc 2)
      )

    t (print "closure-flatten0 can't handle" ast)))

(defun closure-flatten1 (args locals ast)
  (if (list? ast)
    (list (closure-flatten0 args locals ast nil)
          (list :funcall 'list :argc (length ast)))
    (closure-flatten0 args locals ast nil)))

