(include "compiler.lisp")

(def pool (make-hashmap 32))
(def pool-count 1)
(def pool-list nil)

(def method-list (make-hashmap 32))

(defun to-u1 (x)
  (if (< x 0xFF)
    (list x)
    "int too large for 1 byte"))

(defun to-u2 (x)
  (if (or (< x 0xFFFF) (= x 0xFFFF))
    (list (bit-and (bit-asr x 8) 0xFF)
          (bit-and x 0xFF))
    "int too large for 2 bytes"))

(defun to-u4 (x)
  (concat (to-u2 (// x 0x10000))
          (to-u2 (bit-and x 0xFFFF))))

(defun len1-attribs (const-list)
  (to-u2 (nth 1 const-list)))

(defun len2-attribs (const-list)
  (concat (to-u2 (nth 1 const-list))
          (to-u2 (nth 2 const-list))))

; converts a human readable item in the constant pool to list of bytes
(defun const-to-bin (const-list)
  (let (tag (car const-list))
    (cond
      (equal? tag :utf8)
      (concat (cons 0x01 (to-u2 (length (nth 1 const-list))))
               (map char-int (to-chars (nth 1 const-list))))
      
      (equal? tag :classref)    (cons 0x07 (len1-attribs const-list))
      (equal? tag :string)      (cons 0x08 (len1-attribs const-list))
      (equal? tag :fieldref)    (cons 0x09 (len2-attribs const-list))
      (equal? tag :methodref)   (cons 0x0A (len2-attribs const-list))
      (equal? tag :nametyperef) (cons 0x0C (len2-attribs const-list))
      t (list :unknown const-list))))

; adds a human readable listing to the constant pool
(defun pool-add (poolmap attribs)
  (let (tag (car attribs)
        val (cdr attribs))
    (cond
      (equal? tag :utf8)
      (hashmap-set! poolmap attribs nil)
      
      ; class-ref -> utf8 name
      ; string    -> utf8
      (or (equal? tag :classref) (equal? tag :string))
      (let (string-r (list :utf8 (car val)))
        (progn
          (pool-add poolmap string-r)
          (hashmap-set! poolmap
                        attribs
                        string-r)))
      
      ; nametype-ref -> utf8 name, utf8 type
      (equal? tag :nametyperef)
      (let (name-r (list :utf8 (car val))
            type-r (list :utf8 (nth 1 val)))
        (progn
          (pool-add poolmap name-r)
          (pool-add poolmap type-r)
          (hashmap-set! poolmap
                        attribs
                        (list name-r type-r))))

      ; field-ref  -> class, nametype
      ; method-ref -> class, nametype
      (or (equal? tag :fieldref) (equal? tag :methodref))
      (let (class-r    (car val)
            nametype-r (nth 1 val))
        (progn
          (pool-add poolmap class-r)
          (pool-add poolmap nametype-r)
          (hashmap-set! poolmap
                        attribs
                        (list class-r nametype-r))))

      t (hashmap-set! poolmap (cons :unknown attribs))
      )))

; causes all desendants of parent to map to constant pool indicies
(defun pool-resolve (poolmap parent)
  (let (parent-type   (car parent)
        child0        (hashmap-get poolmap parent)
        child-value   (car child0))
    (if (int? child-value)
      child-value
      (progn
        (def pool-list
             (cons
               (cond
                 (not child-value)
                 parent

                 (or (equal? parent-type :classref)
                     (equal? parent-type :string))
                 (list (car parent)
                       (pool-resolve poolmap child-value))

                 (or (equal? parent-type :nametyperef)
                     (equal? parent-type :methodref)
                     (equal? parent-type :fieldref))
                 (list (car parent)
                       (pool-resolve poolmap (nth 0 child-value))
                       (pool-resolve poolmap (nth 1 child-value)))

                 t (list :unknown parent))
               pool-list))
        (hashmap-set! poolmap parent pool-count)
        (def pool-count (inc pool-count))
        (dec pool-count)
        ))))

(defun pool-get (constpool expr)
  (progn
    (if (nil? (nth 1 (hashmap-get constpool expr)))
      (pool-add constpool expr))
    (pool-resolve constpool expr)))

; generates code to store stack onto local args for tail recursion
(defun set-locals (n acc)
  (if (< n 0)
    acc
    (set-locals (dec n) (cons (list :astore n) acc))))

(def method-list
     {
     "rest" (list "Lang" "cdr"
                 "(Ljava/lang/Object;)Ljava/lang/Object;")
     "cons" (list "Lang" "cons"
                  "(Ljava/lang/Object;Ljava/lang/Object;)LConsCell;")
     "equal?" (list "Lang" "isEqual"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
     "inc" (list "Lang" "inc"
                 "(Ljava/lang/Object;)Ljava/lang/Object;")
     "char-at" (list "Lang" "charAt"
                     "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
     "readline" (list "Lang" "readLine"
                      "()Ljava/lang/String;")
     "print" (list "Lang" "print"
                   "(Ljava/lang/Object;)Ljava/lang/Object;")
     "println" ("Lang" "println"
                   "(Ljava/lang/Object;)Ljava/lang/Object;")
     })

(hashmap-set! method-list 
              "println"
              (list "Lang" "println" "(Ljava/lang/Object;)Ljava/lang/Object;"))

(hashmap-set! method-list 
              "char"
              (list "Lang" "toChar" "(Ljava/lang/Object;)Ljava/lang/Object;"))

(hashmap-set! method-list 
              "substr"
              (list "Lang" "substr"
                "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"))

(hashmap-set! method-list 
              "whitespace?"
              (list "Lang" "isWhitespace" "(Ljava/lang/Object;)Ljava/lang/Object;"))

(hashmap-set! method-list 
              "pprint"
              (list "Lang" "pprint" "(Ljava/lang/Object;)Ljava/lang/Object;"))

(hashmap-set! method-list 
              "first"
              (list "Lang" "car" "(Ljava/lang/Object;)Ljava/lang/Object;"))


;(print method-list)

(defun funcall-resolve (expr)
  (let (name (string (second expr))
        ctype (hashmap-get method-list name)
        exists? (second ctype)
        call (first ctype)
        ;_ (print (cons :asdf call))
        ;_ (if (equal? "println" name) (print (cons :asdf call)))
        )
    (if exists?
      (cons :invokestatic call)
      (progn (print "can't find function:") (print name)))))

(defun ir-to-jvm (expr)
  (let (cmd (car expr))
    (cond
      (equal? cmd :return)
      (list :areturn)

      (equal? cmd :push)
      (cond
        (equal? (nth 1 expr) :arg-num) (list :aload (nth 2 expr))
        (equal? (nth 1 expr) :nil)     (list :aconst_null)
        (equal? (nth 1 expr) :true)    (list :getstatic
                                             "java/lang/Boolean"
                                             "TRUE"
                                             "Ljava/lang/Boolean;")
        (equal? (nth 1 expr) :int-const)
        (list (list :invokestatic "java/lang/Integer"
                    "valueOf" "(I)Ljava/lang/Integer;")
              (list :iconst (nth 2 expr)))

        (equal? (nth 1 expr) :char-const)
        (list (list :invokestatic "java/lang/Character"
                    "valueOf" "(C)Ljava/lang/Character;")
              (list :iconst (char-int (nth 2 expr))))

        (equal? (nth 1 expr) :str-const)
        (list :ldc (list :string (nth 2 expr)))

        t (progn (print "can't push: ") (print expr)))

      (equal? cmd :jump-if-nil)
      (cons :ifnull (cdr expr))

      ; defer to jump resolution
      (equal? cmd :label) expr
      (equal? cmd :goto) expr

      (equal? cmd :funcall)
      (funcall-resolve expr)

      (equal? cmd :tail-recur)
      (cons (list :goto :start) (set-locals (dec (nth 2 expr)) nil))

      ; t (progn (print "ir-to-jvm can't compile") (print expr) expr)
      t expr
      )))

; calculates max stack size and size of stack at jump targets
(defun max-stack (in max-c curr-c lablist)
  (if in
    (let (expr (car in)
          cmd (car expr)
          ;_ (progn (print curr-c) (print expr))
          max-c (if (< max-c curr-c) curr-c max-c))
      (cond
        (equal? cmd :push)
        (max-stack (cdr in) max-c (inc curr-c) lablist)

        (equal? cmd :pop)
        (max-stack (cdr in) max-c (dec curr-c) lablist)

        (equal? cmd :funcall)
        (max-stack (cdr in) max-c (inc (- curr-c (nth 3 expr))) lablist)

        (equal? cmd :jump-if-nil)
        ;(max-stack (cdr in) max-c (dec curr-c)
        ;            (cons (list (nth 1 expr) (dec curr-c)) lablist))
        (max-stack (cdr in) max-c (dec curr-c)
                   (hashmap-set! lablist (nth 1 expr) (dec curr-c)))

        (equal? cmd :goto)
        ;(max-stack (cdr in) max-c (dec curr-c)
        ;            (cons (list (nth 1 expr) curr-c) lablist))
        (max-stack (cdr in) max-c (dec curr-c)
                   (hashmap-set! lablist (nth 1 expr) curr-c))

        (equal? cmd :tail-recur)
        ;(max-stack (cdr in) max-c curr-c
        ;            (append lablist (list :start 0)))
        (max-stack (cdr in) max-c (- curr-c (nth 2 expr))
                   ;(hashmap-set! lablist (nth 1 expr) 0))
                   (hashmap-set! lablist :start 0))

        t (max-stack (cdr in) max-c curr-c lablist)))
    ;(list max-c (reverse lablist))
    (list max-c lablist)))

;; converts a single item of the form (:jvmcode args) into a list of bytes
(defun jvm-assemble0 (in)
  (let (cmd (car in))
    (cond
      (equal? cmd :aconst_null)
      (list 0x01)

      (equal? cmd :aload)
      (cons 0x19 (to-u1 (nth 1 expr)))

      (equal? cmd :astore)
      (cons 0x3A (to-u1 (nth 1 expr)))

      (equal? cmd :ldc)
      (cons 0x12 (to-u1 (pool-get pool (nth 1 expr))))

      (equal? cmd :iconst)
      ; iconst_<> -> bipush -> sipush -> ldc
      (let (val (nth 1 in))
        (cond
          (and (< (- 2) val) (< val 6))
          (list (+ val 3)) ; direct constant value

          (and (< (- 129) val) (< val 128))
          (list 0x10 val) ;bipush

          t (print "can't iconst value:" val)))

      (equal? cmd :pop)
      (list 0x57)

      (equal? cmd :areturn)
      (list 0xB0)

      (equal? cmd :return)
      (list 0xB1)

      (equal? cmd :getstatic)
      (cons 0xB2
            (to-u2
              (pool-get pool
                        (list :fieldref
                              (list :classref (nth 1 expr))
                              (list :nametyperef (nth 2 expr)
                                    (nth 3 expr))))))

      (equal? cmd :invokevirtual)
      (cons 0xB6
            (to-u2
              (pool-get pool
                        (list :methodref
                              (list :classref (nth 1 expr))
                              (list :nametyperef (nth 2 expr)
                                    (nth 3 expr))))))

      (equal? cmd :invokestatic)
      (cons 0xB8
            (to-u2
              (pool-get pool
                        (list :methodref
                              (list :classref (nth 1 expr))
                              (list :nametyperef (nth 2 expr)
                                    (nth 3 expr))))))

      (equal? cmd :checkcast)
      (cons 0xC0
            (to-u2
              (pool-get pool (list :classref (nth 1 expr)))))
      ;t (progn (print "jvm-assmble can't assemble") (print expr) expr)
      t expr
      )))

;; resolves labels and compiles to byte lists whenever possible
(defun jvm-assemble1 (in acc offset labelmap)
  (if in
    (let (expr (car in)
          binexpr (jvm-assemble0 expr)
          ; length of command in bytes
          binexprlen (cond
                       ; non-jump related code was compiled to bytes
                       (int? (car binexpr)) (length binexpr)
                       ; 1 byte for command + 2 bytes for offset
                       (equal? (car expr) :ifnull) 3
                       (equal? (car expr) :goto) 3))
      (if (equal? (car expr) :label)
        ; offset does not change, but add offset to labelmap
        (jvm-assemble1 (cdr in) acc offset
                       (hashmap-set! labelmap (nth 1 expr) offset))
        (jvm-assemble1 (cdr in)
                       (cons binexpr acc)
                       (+ offset binexprlen)
                       labelmap)))
  (list (reverse! acc) labelmap)))

;; resolves jumps
(defun jvm-assemble2 (in acc offset labelmap)
  (if in
    (let (expr (car in)
          cmd (car expr))
      (cond
        (equal? cmd :ifnull)
        (jvm-assemble2 (cdr in)
                       (cons (cons
                               0xC6
                               (to-u2 (- (car (hashmap-get labelmap (nth 1 expr)))
                                         offset)))
                             acc)
                       (+ offset 3)
                       labelmap)

        (equal? cmd :goto-0)
        (progn (print (to-u2 (- offset)))
        (jvm-assemble2 (cdr in)
                       (cons (cons 0xA7 (to-u2 (- offset))) acc)
                       (+ offset 3)
                       labelmap))

        (equal? cmd :goto)
        (jvm-assemble2 (cdr in)
                       (cons (cons
                               0xA7
                               (to-u2 (- (car (hashmap-get labelmap (nth 1 expr)))
                                         offset)))
                             acc)
                       (+ offset 3)
                       labelmap)

        t (jvm-assemble2 (cdr in)
                         (cons expr acc)
                         (+ offset (length expr))
                         labelmap)))
    (reverse! acc)))

(defun jvm-assemble (in)
  (let (bin-and-labels (jvm-assemble1 in nil 0 (make-hashmap 32))
        bin-list (car bin-and-labels)
        labelmap (nth 1 bin-and-labels))
    (jvm-assemble2 bin-list nil 0 labelmap)))

;; stack frame object entry of class Object
(def objvar-info
     (list 0x07 (to-u2 (pool-get pool (list :classref "java/lang/Object")))))

(defun sframe (offset localcount stackcount)
  (if (= stackcount 0)
    (progn
      ;(print "same frame") (print offset)
    (list offset) ; same frame type (same locals, zero on stack)
    )
    (progn
      ;(print "full frame") (print localcount) (print stackcount)
    (list 0xFF ; full frame type
          (to-u2 offset)
          (if (= localcount 0)
            (quote ())
            (list (to-u2 localcount) (repeat objvar-info localcount)))
          (to-u2 stackcount)
          (repeat objvar-info stackcount)))))

(defun sframe-resolve0 (argc offsets stacklabs bytepos acc)
  (if offsets
    (let (frameoff (second (first offsets)))
          ;_ (print (dec (- frameoff bytepos)))
          ;_ (print frameoff))
      (if (second (hashmap-get stacklabs (first (first offsets))))
        (sframe-resolve0
          argc (cdr offsets) stacklabs frameoff
          (cons (sframe (dec (- frameoff bytepos)) argc
                        (first (hashmap-get stacklabs (first (first offsets)))))
                acc))
        (sframe-resolve0 argc (cdr offsets) stacklabs bytepos acc)))
    (reverse! acc)))

(defun sframe-resolve (argc labels label-offsets)
  (let (offsets (keyvals label-offsets))
    (sframe-resolve0 argc
      (qsort
        (lambda (a b) (- (second a) (second b))) offsets)
      labels (- 1) nil)))

(defun check-tco0 (ir acc)
  (if ir
    (if (equal? (car (car ir)) :tail-recur)
      (check-tco0 (cdr ir) ;(cdr (cdr ir))
                  (cons (car ir)
                        (if (equal? (last acc) (list :label :start))
                          acc
                          (append acc (list :label :start)))
                        ))
      (check-tco0 (cdr ir) (cons (car ir) acc)))
  (reverse acc)))

(defun method-type (argc)
  (reduce str-cat (cons "(" (repeat0 "Ljava/lang/Object;" argc
                                     (list ")Ljava/lang/Object;")))))

(defun compile1 (name args expr)
  (let (_ (print name)
        raw-ir (ir (symbol name) expr)
        ;; allow for infinite tail recursion
        raw-ir (if (equal? (first (last raw-ir)) :tail-recur)
                 raw-ir (append raw-ir (list :return)))
        ir-list (check-tco0 raw-ir nil)
        ;_ (map print ir-list)
        ; human readable jvm bytecode
        jvm-asm (semi-flatten (map ir-to-jvm (resolve-syms ir-list args)))
        ; _ (map print jvm-asm)
        ; unresolved jump binary + label -> offset map
        bin-and-labels (jvm-assemble1 jvm-asm nil 0 (make-hashmap 32))
        ; binary with resolved jumps
        labelmap (nth 1 bin-and-labels)
        ; _ (print "?")
        ;_ (map print bin-and-labels)
        jvm-bin (jvm-assemble2 (car bin-and-labels) nil 0 labelmap)
        ;_ (print "?")
        bytecode (flatten jvm-bin) ; bytecode
        code-size (length bytecode)

        argc (length args)
        _ (hashmap-set! method-list (string name)
                        (list "Lateral" (string name) (method-type argc)))
        ; stack-info (max-stack ir-list 0 0 nil)
        ;_ (print "?")
        ;_ (map print ir-list)
        stack-info (max-stack ir-list 0 0 (make-hashmap 16))
        ;_ (print "?")
        stack-size (car stack-info)
        ;_ (progn (print "stack height") (print (nth 1 stack-info)))
        ;_ (progn (print "label pos") (print labelmap))
        ; _ (print "===")
        stack-frames (sframe-resolve argc (nth 1 stack-info) labelmap)
        ;_ (print stack-frames)
        flat-stack-map (flatten stack-frames)
        stack-map-frames-size (length flat-stack-map)
        )
    (list
      0x00 0x09 ; public static
      ; name of function
      (to-u2 (pool-get pool (list :utf8 name)))
      ; type signature
      (to-u2 (pool-get pool (list :utf8 (method-type argc))))
      0x00 0x01 ; attribute size of 1
      (to-u2 (pool-get pool (list :utf8 "Code")))
      (if stack-frames
        (to-u4 (+ 12 code-size 8 stack-map-frames-size))
        (to-u4 (+ 12 code-size)))
      (to-u2 stack-size)
      (to-u2 argc)
      (to-u4 code-size)
      bytecode
      0x00 0x00 ; 0 exceptions
      (if stack-frames
        (list 0x00 0x01 ; one StackMapTable attribute
              (to-u2 (pool-get pool (list :utf8 "StackMapTable")))
              ; length in bytes = number of frames (u2) + length of binary
              (to-u4 (+ 2 (length flat-stack-map)))
              (to-u2 (length stack-frames)) ; number of frames
              stack-frames)
        ; no attributes if StackMapTable is empty
        (list 0x00 0x00)))))

(defun constbin-dbg (in acc)
  (if in
  (progn
    (print (car in))
    (constbin-dbg (cdr in) (cons (const-to-bin (car in)) acc)))
  acc))

(defun class-headers (name parent methods)
  (progn
    ; add class and parent to constant pool
    (pool-get pool (list :classref name))
    (pool-get pool (list :classref parent))
    ; (map print (reverse pool-list))
    (list
      0xCA 0xFE 0xBA 0xBE ; java magic number
      0x00 0x00 0x00 0x37 ; java version 55.0 (Java 11)
      (to-u2 (inc (length pool-list)))
      (flatten (map const-to-bin (reverse pool-list)))
      ;(flatten (constbin-dbg (reverse pool-list) nil))
      0x00 0x21 ; extendable (not final) and public
      (to-u2 (pool-get pool (list :classref name)))
      (to-u2 (pool-get pool (list :classref parent)))
      0x00 0x00 ; zero interfaces
      0x00 0x00 ; zero fields
      (to-u2 (length methods))
      methods
      0x00 0x00))) ; zero attributes

(def funlist nil)
(defun compile-function0 (name args expr)
  (def funlist (cons (compile1 (string name) args expr) funlist)))

(defmacro defun (name args expr)
  (list (quote compile-function0)
        (list (quote quote) name)
        (list (quote quote) args)
        (list (quote quote) expr)))

(include "core2.lisp")

; (map print pool-list)
(write-bytes
  "Lateral.class"
  (flatten
    (class-headers
      "Lateral"
      "java/lang/Object"
      funlist)))
