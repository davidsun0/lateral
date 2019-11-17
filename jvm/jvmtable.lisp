;(def bytecodes (hashmap 256))
(def bytecodes (hashmap))

(insert! bytecodes :aconst_null 0x01)

(insert! bytecodes :ldc 0x12)
(insert! bytecodes :ldc_w 0x13)
(insert! bytecodes :aload 0x19)

(insert! bytecodes :astore 0x3A)

(insert! bytecodes :pop 0x57)
(insert! bytecodes :dup 0x59)

(insert! bytecodes :goto 0xA7)

(insert! bytecodes :areturn 0xB0)
(insert! bytecodes :return 0xB1)

(insert! bytecodes :getstatic 0xB2)

(insert! bytecodes :invokevirtual 0xB6)
(insert! bytecodes :invokestatic 0xB8)

(insert! bytecodes :checkcast 0xC0)

(insert! bytecodes :ifnull 0xC6)
(insert! bytecodes :ifnonnull 0xC7)

;; core method list
(def method-list (hashmap))

(defun method-type (argc)
  (->> (list ")Ljava/lang/Object;")
       (repeat0 "Ljava/lang/Object;" argc)
       (cons "(")
       (apply string)))

(defun insert-method (sym class name argc)
  (insert! method-list sym 
           (list class name (method-type argc))))

(insert-method "first" "Lang" "car" 1)
(insert-method "rest" "Lang" "cdr" 1)
(insert-method "cons0" "Lang" "cons0" 2)

(insert! method-list "cons"
         (lambda (n)
           (-> (list :funcall (quote cons0) :argc 2)
               (funcall-resolve)
               (repeat (dec n)))))

(insert! method-list "list"
         (lambda (n)
           (cons (list :aconst_null)
                 (-> (list :funcall (quote cons0) :argc 2)
                     (funcall-resolve)
                     (repeat n)))))

(insert-method "add0" "Lang" "add0" 2)

(insert! method-list "+"
         (lambda (n)
           (-> (list :funcall (quote add0) :argc 2)
               (funcall-resolve)
               (repeat (dec n)))))

(insert-method "contains?" "Lang" "contains_p" 2)

(insert-method "equal?" "Lang" "equal_p" 2)

(insert-method "char" "Lang" "to_char" 1)
(insert-method "char-at" "Lang" "char_at" 2)
(insert-method "substr" "Lang" "substr" 3)
(insert-method "whitespace?" "Lang" "whitespace_p" 1)

(insert-method "pprint" "Lang" "pprint" 1)
(insert-method "println" "Lang" "println" 1)
(insert-method "print" "Lang" "println" 1)

(insert-method "dec" "Lang" "dec" 1)
(insert-method "inc" "Lang" "inc" 1)
(insert-method "=" "Lang" "isNumericallyEqual" 2)

(insert-method "slurp" "Lang" "slurp" 1)
(insert-method "readline" "Lang" "readLine" 0)
;(insert-method "read-atom" "Helper" "readAtom" 1)
(insert-method "read-atom0" "Helper" "readAtom" 1)
(insert-method "make-lambda" "Lang" "lambda" 2)
(insert-method "make-macro" "Lang" "macro" 2)

(insert-method "make-envir" "Lang" "make_envir" 1)
(insert-method "user-envir" "Runtime" "getUserEnvir" 0)
(insert-method "envir-set" "Runtime" "envir_set" 2)
(insert-method "envir-get" "Runtime" "envir_get" 1)

(insert-method "native-invoke" "Lang" "nativeInvoke" 2)

(insert-method "list?" "Lang" "list_p" 1)
(insert-method "symbol?" "Lang" "symbol_p" 1)
(insert-method "lambda?" "Lang" "lambda_p" 1)
(insert-method "native-fn?" "Lang" "native_p" 1)
(insert-method "macro?" "Lang" "macro_p" 1)

(insert-method "insert!" "Lang" "insert_b" 3)
(insert-method "get" "Lang" "get" 2)

(insert-method "get-args" "Lang" "get_args" 1)
(insert-method "get-expr" "Lang" "get_expr" 1)
