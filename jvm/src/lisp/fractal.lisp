(defun adder (n)
  (function (x) (+ x n)))

;; repeatedly applies a function on its output, starting with acc for n times
(defun repeat (f acc n)
  (if (> n 0)
    (recur f (f acc) (dec n))
    acc))

(defmacro def-lsystem (name :rest rules)
  (let (start (gensym)
        iters (gensym)
        input (gensym)
        farg  (gensym))
    `(defun ,name (,start ,iters)
       (repeat (function (,input)
                 (apply concat
                        (map
                          ; transforms every element according to given rules
                          (function (,farg)
                            (case ,farg
                              ,@rules
                              ,farg))
                          ,input)))
               ,start
               ,iters))))

(def-lsystem
  algae2
  :a (list :a :b)
  :b (list :a))

;(time (map (function (x) (length (algae2 (list :a) x))) (range 0 30)))

(def test (hashmap :a 1 :b 2 :c 5))

(print test)
(get test :b)
(assoc test :c 3)
test

(pairs '())
(hashmap)

(range 0 10)
(zip (range 0 10)
     (map even? (range 0 10)))

(macroexpand '(cond a b c d e))
(macroexpand '(cond a b c d))

(print "and nil nil = ")
(and nil nil)
(zip '(a b c) '(e f g))
(and (empty? '(1)) (empty? '(2)))
