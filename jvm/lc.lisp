(defun ir (name args ast)
  (->> (ir0 name args nil ast)
       (semi-flatten)
       (reverse)
       (filter first)))
;(include "ir.lisp")

(include "jvmtable.lisp")

(defun u1 (x)
  (if (< x 0x100)
    (list x)
    (throw "int too large for 1 byte")))

(defun u2 (x)
  (if (or (< x 0xFFFF) (= x 0xFFFF))
    (list (bit-and (bit-asr x 8) 0xFF)
          (bit-and x 0xFF))
    (throw "int too large for 2 bytes")))

(defun u4 (x)
  (concat (u2 (// x 0x10000))
          (u2 (bit-and x 0xFFFF))))

(defun len1-attribs (const-list)
  (u2 (nth 1 const-list)))

(defun len2-attribs (const-list)
  (concat (u2 (nth 1 const-list))
          (u2 (nth 2 const-list))))

(defun const-to-bin (const-list)
  (case (first const-list)
    :utf8 (cons 0x01 (u2 (length (to-chars (second const-list))))
                (map integer (to-chars (second const-list))))
    :classref    (cons 0x07 (len1-attribs const-list))
    :string      (cons 0x08 (len1-attribs const-list))
    :fieldref    (cons 0x09 (len2-attribs const-list))
    :methodref   (cons 0x0A (len2-attribs const-list))
    :nametyperef (cons 0x0C (len2-attribs const-list))
    (list :unknown const-list)))

(defun pool-search! (constpool expr)
  (if (contains? constpool expr)
    (first (get constpool expr))
    (let (pool-count (get constpool :count 1))
      (progn
        (insert! constpool expr pool-count)
        (insert! constpool :count (inc pool-count))
        pool-count))))

(defun pool-get! (constpool expr)
  (let (expr (if (string? expr) (list :utf8 expr) expr)
        tag (first expr))
    (case tag
      :utf8 (pool-search! constpool expr)

      (:classref :string)
      (pool-search!
        constpool
        (list tag (pool-get! constpool (string (second expr)))))

      (:nametyperef :methodref :fieldref)
      (pool-search!
        constpool
        (list tag
              (pool-get! constpool (second expr))
              (pool-get! constpool (third expr))))

      :getstatic
      (pool-search!
        constpool
        (list tag
              (pool-get! constpool (second expr))
              (pool-get! constpool (third expr))
              (pool-get! constpool (fourth expr))))

      (throw "can't pool-get" expr tag "test"))))

; generates code to store stack onto local args for tail recursion
(defun set-locals (n i acc)
  (if (< n i)
    acc
    (set-locals n (inc i) (cons (list :astore i) acc))))

(defun ir-to-jvm (expr)
    (case (first expr)
      :return (list :areturn)

      :push
      (case (nth 1 expr)
        :arg-num (list :aload (nth 2 expr))
        :nil     (list :aconst_null)
        :true    (list :getstatic
                       "java/lang/Boolean"
                       "TRUE"
                       "Ljava/lang/Boolean;")
        :int-const
        (list (list :iconst (nth 2 expr))
              (list :invokestatic "java/lang/Integer"
                    "valueOf" "(I)Ljava/lang/Integer;"))

        :char-const
        (list (list :iconst (integer (nth 2 expr)))
              (list :invokestatic "java/lang/Character"
                    "valueOf" "(C)Ljava/lang/Character;"))

        :str-const
        (list :ldc (list :string (nth 2 expr)))

        :symbol
        (list (list :ldc (list :string (string (nth 2 expr))))
              (list :invokestatic "Symbol" "makeSymbol"
                    "(Ljava/lang/String;)LSymbol;"))

        :keyword
        (list (list :ldc (list :string (string (nth 2 expr))))
              (list :invokestatic "Keyword" "makeKeyword"
                    "(Ljava/lang/String;)LKeyword;"))

        (progn (print "ir-to-jvm: can't push" expr) (throw "error")))

      :store        (list :astore (second expr))
      :jump-if-nil  (cons :ifnull (rest expr))
      :jump-not-nil (cons :ifnonnull (rest expr))
      :tail-recur   (set-locals (dec (nth 2 expr)) 0 (list (list :goto :start)))
      ;TODO: figure out how to call function objects
      ;:dynamcall    (funcall-resolve (cons :funcall "invoke" (rest expr)))
      expr))

;; prepends start label to ir if there are tail recursive calls
(defun check-tco0 (in curr)
  (cond
    (nil? curr) in
    (equal? (first (first curr)) :tail-recur) (cons (list :label :start) in)
    t (check-tco0 in (rest curr))))

(defun check-tco (in)
  (check-tco0 in in))

(defun funcall-resolve (method-list expr)
  (if (equal? (first expr) :funcall)
    (let (call (->> (second expr)
                    (string)
                    (get method-list)
                    (first)))
      (cond
        (nil? call) (print "funcall-resolve can't resolve " call expr)
        (lambda? call) (call method-list (fourth expr))
        (list? call) (cons :invokestatic call)
        t (list :invokestatic call)))
    expr))

;; converts a single item of the form (:jvmcode args) into a list of bytes
(defun jvm-assemble (in)
  (case (first in)
    ;; simple bytecode ops
    (:aconst_null :pop :dup :areturn :return :nop)
    (list (get *bytecodes* (first in) nil))

    :aload
    (if (< (second in) 4)
      (list (+ 0x2A (second in)))
      (list 0x19 (second in)))

    :astore
    (if (< (second in) 4)
      (list (+ 0x4B (second in)))
      (list 0x3A (second in)))

    :iconst
    ; iconst_<> -> bipush -> sipush -> ldc
    (let (val (second in))
      (cond
        ; iconst literal
        (and (< (- 2) val) (< val 6))       (list (+ val 3))
        ; bipush
        (and (< (- 129) val) (< val 128))   (list 0x10 val)
        t (print "can't iconst value:" val)))

    :checkcast
    (->> (second in)
         (list :classref)
         (pool-get! pool)
         (u2)
         (cons 0xC0))

    in))

(defun pool-resolve! (pool in)
  (case (first in)
    (:invokestatic :invokevirtual)
    (->> (list :methodref 
               (list :classref
                     (second in))
               (list :nametyperef
                     (third in)
                     (fourth in)))
         ;(print-iden)
         (pool-get! pool)
         (u2)
         (cons (first (get *bytecodes* (first in)))))

    :getstatic
    (->> (nth 3 in)
         (list :nametyperef (third in))
         (list :fieldref
               (list :classref (second in)))
         (pool-get! pool)
         (u2)
         (cons 0xB2))

    :ldc
    (let (idx (pool-get! pool (second in)))
      (if (< idx 0x100)
        (cons 0x12 (u1 idx))
        (cons 0x13 (u2 idx))))

    in))

;; calculates label byte offsets
(defun label-resolve0! (a b)
  (let (labelmap (first a)
        offset   (second a))
    (if (index (first b) (list :label :local-count :let-pop))
      (list (insert! labelmap (second b) offset)
            offset)
      (list labelmap
            (+ offset
               (cond
                 (int? (first b)) (length b)
                 (index (first b) (list :ifnull :ifnonnull :goto)) 3
                 t (print "unknown label: " b)))))))

(defun label-resolve (bytes)
  (first (foldl label-resolve0! (list (hashmap) 0) bytes)))

;; resolves jumps, associates labels with byte offsets
(defun jump-resolve0 (in acc offset labelmap)
  (let (expr (first in)
        cmd  (first expr))
    (cond
      (nil? in) (reverse acc)

      (index cmd (list :ifnull :ifnonnull :goto))
      (jump-resolve0 (rest in)
                     (cons (cons
                             (get *bytecodes* cmd nil)
                             (u2 (- (get labelmap (second expr) nil) offset)))
                           acc)
                     (+ offset 3)
                     labelmap)

      t (jump-resolve0 (rest in)
                       (cons expr acc)
                       (+ offset
                          (if (int? cmd) (length expr) 0))
                       labelmap))))

(defun jump-resolve (jvm-asm)
  (->> (label-resolve jvm-asm)
       (jump-resolve0 jvm-asm nil 0)
       (filter (lambda (x) (int? (first x))))))

;; quick and dirty way to get argc from type string
;; should count number of matches for this regex
;; "\([ILFDC(L.*;)]*\)"
(defun count-semi (str idx acc)
  (let (c (char-at str idx))
    ;; do not convert to case. there is a bug with nil
    (cond
      (index c (list nil (to-char ")"))) acc
      (index c (list (to-char "I") (to-char ";"))) (count-semi str (inc idx) (inc acc))
      t (count-semi str (inc idx) acc))))

(defun local-info0 (acc expr)
  (let (mloc    (first acc)
        lablist (second acc))
    (case (first expr)
      ;; set based on local tag info
      (:local-count :let-pop)
      (list (third expr)
            (insert! lablist (second expr) (third expr)))

      (:ifnull :ifnonnull :goto)
      (list mloc
            (if (equal? (second expr) :start)
              lablist
              (insert! lablist (second expr) mloc)))
     
      (list mloc lablist))))

(defun local-info (argc jvm-asm)
  (insert!
    (second (foldl local-info0 (list argc (hashmap)) jvm-asm))
    :start argc))

(defun stack-info0 (acc expr)
  (let (cstack (second acc)
        mstack (if (< (first acc) cstack)
                 cstack
                 (first acc))
        lablist (third acc)
        )
    (case (first expr)
      ;; stack +1
      (:aload :aconst_null :iconst :dup :getstatic :ldc)
      (list mstack (inc cstack) lablist)

      ;; stack -1
      ;; gotos decrease the stack when jumping to another branch
      ;; gotos to start keep the stack the same
      (:areturn :pop :astore :ifnull :ifnonnull :goto)
      (list mstack
            (if (equal? expr (quote (:goto :start)))
              cstack
              (dec cstack))
            (cond
              (equal? (first expr) :goto)
              (insert! lablist (second expr) cstack)

              (index (first expr) (list :goto :ifnull :ifnonnull))
              (insert! lablist (second expr) (dec cstack))

              t lablist))

      ;; stack +- 0
      (:label :return :let-pop :local-count)
      (list mstack cstack
            (if (equal? (first expr) :goto)
              (insert! lablist (second expr) cstack)
              lablist))

      ;; stack - argc + 1
      :invokestatic
      (list mstack
            (- cstack (dec (count-semi (nth 3 expr) 0 0)))
            lablist)

      (progn 
        (print "stack-info0 can't handle " expr)
        (list mstack cstack lablist)))))

;; stack frame object entry of class Object
(defun objvar-info (pool)
  (->> "java/lang/Object"
       (list :classref)
       (pool-get! pool)
       (u2)
       (cons 0x07)))

(defun sframe1 (offset last-local l-count s-count)
  (let (obj (objvar-info pool))
  (cond
    ; same frame
    (and (= last-local l-count) (= s-count 0) (< offset 64))
    (list offset)

    ;; these work in theory but not in practice
    ; same locals 1 stack item
    ;(and (= last-local l-count) (= s-count 1) (< offset 64))
    ;(list (+ offset 64) objvar-info)

    ; same locals 1 stack item extended
    ;(and (= last-local l-count) (= s-count 1))
    ;(list 247 (to-u2 offset) objvar-info)

    ; chop frame
    ;(and (= s-count 0) (< (- last-local l-count)

    ; full frame
    t (list 0xFF (u2 offset)
            (if (= l-count 0)
              (list (u2 0))
              (list (u2 l-count) (repeat obj l-count)))
            (if (= s-count 0)
              (list (u2 0))
              (list (u2 s-count) (repeat obj s-count)))))))

(defun sframe-resolve0 (poff ploc acc lstat)
  (progn ;(print poff ploc acc lstat)
  (let (item (first lstat)
        offset (first item)
        stacks (second item)
        locals (third item))
    (if (nil? lstat)
      (reverse acc)
      (sframe-resolve0
        offset
        locals
        (cons (sframe1 (dec (- offset poff)) ploc locals stacks) acc)
        (rest lstat))))))

(defun sframe-resolve (argc offsets stack-i local-i)
  (->> (keyvals offsets)
       (msort (lambda (a b) (< (second a) (second b))))
       (print-iden)
       (map (lambda (x)
              (list (second x)
                    (get stack-i (first x) nil)
                    (get local-i (first x) nil))))
       (filter (lambda (x) (second x)))
       (sframe-resolve0 (- 1) argc nil)))

(defun arg-count (args)
  (if (index :rest args)
    (inc (index :rest args))
    (length args)))

(defun compile0! (pool method-list name args jvm-asm signature)
  (let (_ (pprint)
        _ (print name)
        _ (print "asm:")
        _ (map print jvm-asm)
        ; human readable jvm bytecode
        _ (print "====")
        bytecode (->> jvm-asm
                      (map jvm-assemble)
                      (map (lambda (x) (pool-resolve! pool x)))
                      (jump-resolve)
                      (flatten))
        _ (print "bytecode: " bytecode)
        label-i (->> jvm-asm
                     (map jvm-assemble)
                     (map (lambda (x) (pool-resolve! pool x)))
                     (label-resolve))
        _ (print "label info: " label-i)
        local-i (local-info (arg-count args) jvm-asm)
        _ (print "local info: " local-i)
        stack-i (foldl stack-info0 (list 0 0 (hashmap)) jvm-asm)
        _ (print "stack info: " stack-i)
        sframes (sframe-resolve (arg-count args) label-i (third stack-i) local-i)
        _ (print "stack frames: " sframes)
        )
    (list
      0x00 0x09 ; public static
      ; name of function
      (u2 (pool-get! pool (string name)))
      ; type signature
      (u2 (pool-get! pool signature))
      0x00 0x01 ; attribute size of 1
      (u2 (pool-get! pool "Code"))
      (u4 (+ 12 (length bytecode)
             (if sframes
               (+ 8 (length (flatten sframes)))
               0)))
      (u2 (first stack-i)) ; max stack height
      (u2 (let (maxloc (apply max (map second (keyvals local-i))))
            (if maxloc
              maxloc
              (arg-count args))))
      (u4 (length bytecode))
      bytecode
      0x00 0x00 ; 0 exceptions
      (if sframes
        (list 0x00 0x01 ; one attribute (StackMapTable)
              (u2 (pool-get! pool (list :utf8 "StackMapTable")))
              ; length in bytes = size(number of frames) + length of binary
              (u4 (+ 2 (length (flatten sframes))))
              (u2 (length sframes)) ; number of frames
              sframes)
        ; no attributes if StackMapTable is empty
        (list 0x00 0x00)))))

(defun compile1! (cpool method-list defnexpr)
  (let (name (second defnexpr)
        args (third defnexpr)
        expr (fourth defnexpr))
    (compile0! 
      cpool
      method-list
      name
      args
      (->> (ir name (filter symbol? args) expr)
           (check-tco)
           (map ir-to-jvm)
           (semi-flatten)
           (map (lambda (x) (funcall-resolve method-list x)))
           (semi-flatten))
      (method-type (arg-count args)))))

(defun compile-special! (cpool method-list name args body signature)
  (compile0!
    cpool
    method-list
    name
    args
    (->> (ir nil args body)
         (map ir-to-jvm)
         ((lambda (x) (append x (list :return))))
         (semi-flatten)
         (map (lambda (x) (funcall-resolve method-list x)))
         (semi-flatten))
    signature))

(defun class-headers (pool name parent methods)
  (let (; add class and parent to constant pool
        class-idx (pool-get! pool (list :classref name))
        parent-idx (pool-get! pool (list :classref parent))
        const-pool-size (get pool :count nil)
        ;_ (map print (keyvals pool))
        )
    (list
      0xCA 0xFE 0xBA 0xBE ; java magic number
      0x00 0x00 0x00 0x37 ; java version 55.0 (Java 11)
      (u2 const-pool-size)
      (->> (keyvals pool)
           (filter (lambda (x) (list? (first x))))
           (msort (lambda (a b) (< (second a) (second b))))
           (map first)
           (map const-to-bin)
           (flatten))
      0x00 0x21 ; extendable (not final) and public
      (u2 class-idx)
      (u2 parent-idx)
      0x00 0x00 ; zero interfaces
      0x00 0x00 ; zero fields
      (u2 (length methods))
      methods
      0x00 0x00))) ; zero attributes

;(include "read.lisp")

(defun call-split (in funs macros etc)
  (cond
    (nil? in) (list (reverse funs)
                    (reverse macros)
                    (reverse etc))

    (equal? (first (first in)) (quote defun))
    (call-split (rest in) (cons (first in) funs) macros etc)

    (equal? (first (first in)) (quote defmacro))
    (call-split (rest in) funs (cons (first in) macros) etc)

    t
    (call-split (rest in) funs macros (cons (first in) etc))))

(defun read-funs (path)
  (first (call-split (read-all path) nil nil nil)))

(defun compile2 (path classname)
  (let (exprs (read-all path)
        _ (print "done reading")
        splits (call-split exprs nil nil nil)
        _ (print "funs====")
        ;_ (map print funs)
        macros (second splits)
        _ (print "macros===")
        ;_ (map print macros)
        etc (third splits)
        _ (print "other===")
        ;_ (map print etc)
        ; how to expand macros within macros?
        macro-list (hashmap)
        _ (map
            (lambda (x) 
              (progn
                (insert! macro-list (second x)
                         (eval (list (quote lambda) (third x) (nth 3 x))))
                ))
            macros)
        _ (print macro-list)
        ;; macroexpand
        funs (first splits)
        funs (map (lambda (x) (comp-expand x macro-list)) funs)
        _ (print "expanded funs====")
        _ (map print funs)
        cpool (hashmap)
        ;; insert all functions into method-list
        _ (map (lambda (x) (insert-lambda! method-list classname x)) funs)
        cinitbin (compile-special! cpool method-list "<clinit>" nil
                           (cons (quote progn) etc)
                           "()V")
        mainbin 
        ;(if (get method-list "main" nil)
          (compile-special! cpool method-list "main" (quote (0))
                                 (quote (main))
                                 "([Ljava/lang/String;)V");)
        ;x (throw asdf)
        )
    (->> funs
         (map (lambda (x) (compile1! cpool method-list x)))
         (cons cinitbin mainbin)
         (class-headers cpool classname "java/lang/Object")
         (flatten)
         (write-bytes (string classname ".class")))))

(defun comp-expand (in macros)
  (if (list? in)
    (comp-expand0 in nil macros)
    in))

(defun comp-expand0 (in acc macros)
  (cond
    (nil? in) (reverse acc)

    (and (nil? acc) (equal? (first in) (quote quote)))
    in

    (and (nil? acc) (get macros (first in) nil))
    (comp-expand
      (apply (get (user-envir) (first in) nil) (rest in))
      macros)

    t (comp-expand0 (rest in)
                    (cons
                      (comp-expand (first in) macros)
                      acc)
                    macros)))

;; method adding for :rest params
;(lambda (n)
  ;(cons (funcall-resolve (list :funcall (quote list) :argc (- l n)))
        ;(list :funcall (quote name-symbol) :argc l)))

;(eval
  ;`(lambda (n)
     ;(cons (funcall-resolve (:funcall list :argc (- ,l n)))
           ;(:funcall ,name :argc ,l))))

(defun make-params (class name n-params)
  (eval
    (list (quote lambda)
          (quote (ml n))
          (list (quote append)
                (list (quote funcall-resolve)
                      (quote ml)
                      (list (quote list)
                            :funcall
                            "list"
                            :argc
                            (list (quote -)
                                  (quote n)
                                  n-params)))
                (list (quote list)
                      :invokestatic
                      class
                      name
                      (method-type (inc n-params)))))))

(defun insert-lambda! (method-list classname lamb)
  (let (string-name (string (second lamb)))
    (if (index :rest (third lamb))
      (->> (third lamb)
           (index :rest)
           (make-params classname string-name)
           (insert! method-list string-name))
      (->> (third lamb)
           (length)
           (method-type)
           (list classname string-name)
           (insert! method-list string-name)))))

(defun reload ()
  (progn
    (def mlist method-list)
    (map (lambda (x) (insert-lambda! mlist "MyClass" x)) funs)))
