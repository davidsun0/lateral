(include "ir.lisp")
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
    :utf8 (concat (cons 0x01 (u2 (length (second const-list))))
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
              (pool-get! constpool (nth 2 expr))))

      :getstatic
      (pool-search!
        constpool
        (list tag
              (pool-get! constpool (second expr))
              (pool-get! constpool (nth 2 expr))
              (pool-get! constpool (nth 3 expr))))

      (throw "can't pool-get" expr tag "test"))))

(defun funcall-resolve (expr)
  (let (name (string (second expr)) ; function name
        call (get method-list name nil)) ; call and exists
    (cond
      (nil? call) (progn (print "can't find function:") (print name))
      (lambda? call) (call (nth 3 expr))
      t (cons :invokestatic call))))

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
      :funcall      (funcall-resolve expr)
      :tail-recur   (set-locals (dec (nth 2 expr)) 0 (list (list :goto :start)))
      expr))

;; converts a single item of the form (:jvmcode args) into a list of bytes
(defun jvm-assemble0 (in)
  (case (first in)
    ;; simple bytecode ops
    (:aconst_null :pop :dup :areturn :return)
    (get bytecodes (first in) nil)

    :aload
    (if (< (nth 1 expr) 4)
      (+ 0x2A (nth 1 expr))
        (list 0x19 (nth 1 expr)))

    :astore
    (if (< (nth 1 expr) 4)
      (+ 0x4B (nth 1 expr))
      (list 0x3A (nth 1 expr)))

    :ldc
    (let (idx (pool-get! pool (nth 1 expr)))
      (if (< idx 0x100)
        (cons 0x12 (u1 idx))
        (cons 0x13 (u2 idx))))

    :iconst
    ; iconst_<> -> bipush -> sipush -> ldc
    (let (val (nth 1 in))
      (cond
        ; iconst literal
        (and (< (- 2) val) (< val 6)) (list (+ val 3))
        ; bipush
        (and (< (- 129) val) (< val 128)) (list 0x10 val)
        t (print "can't iconst value:" val)))

    (:invokestatic :invokevirtual)
    (->> (list :methodref 
               (list :classref (nth 1 expr))
               (list :nametyperef (nth 2 expr)
                       (nth 3 expr)))
         (pool-get! pool)
         (u2)
         (cons (first (get bytecodes (first expr)))))

    :getstatic
    (->> (nth 3 expr)
         (list :nametyperef (nth 2 expr))
         (list :fieldref
               (list :classref (nth 1 expr)))
         (pool-get! pool)
         (u2)
         (cons 0xB2))

    :checkcast
    (->> (nth 1 expr)
         (list :classref)
         (pool-get! pool)
         (u2)
         (cons 0xC0))

    expr))

;; resolves labels and compiles to byte lists whenever possible
(defun jvm-assemble1 (in acc offset labelmap)
  (if in
    (let (expr (first in)
          ;_ (print "expr:" expr)
          binexpr (jvm-assemble0 expr)
          binexpr (if (int? binexpr)
                    (list binexpr)
                    binexpr)
          ;_ (print "binexpr:" binexpr)
          ; length of command in bytes
          binexprlen (cond
                       ; non-jump related code was compiled to bytes
                       (int? (first binexpr)) (length binexpr)
                       ; 1 byte for command + 2 bytes for offset
                       (equal? (first expr) :ifnull) 3
                       (equal? (first expr) :ifnonnull) 3
                       (equal? (first expr) :goto) 3)
          ;_ (print "bin len:" binexprlen)
          ;_ (print "offset:" offset)
          )
      (case (first expr)
        :label
        ; offset does not change, but add offset to labelmap
        (jvm-assemble1 (rest in) acc offset
                       (insert! labelmap (nth 1 expr) offset))

        :local-count
        (jvm-assemble1 (rest in) acc offset
                       (insert! labelmap (nth 1 expr) offset))
        
        :let-pop
        (jvm-assemble1 (rest in) acc offset
                       (insert! labelmap (nth 1 expr) offset))

        (jvm-assemble1 (rest in)
                       (cons binexpr acc)
                       (+ offset binexprlen)
                       labelmap)))
    (list (reverse acc) labelmap)))

;; resolves jumps
(defun jvm-assemble2 (in acc offset labelmap)
  (let (expr (first in)
        cmd  (first expr))
    (cond
      (nil? in) (reverse acc)

      (list-contains? (list :ifnull :ifnonnull :goto) cmd)
      (progn ;(print (- (get labelmap (second expr) nil) offset))
             ;(print "asdf")
      (jvm-assemble2 (rest in)
                     (cons (u2 (- (get labelmap (second expr) nil) offset))
                           (get bytecodes cmd nil)
                           acc)
                     (+ offset 3)
                     labelmap))

      t (jvm-assemble2 (rest in) (cons expr acc)
                       (+ offset (length expr)) labelmap))))

(defun jvm-assemble (in)
  (let (bin-and-labels (jvm-assemble1 in nil 0 (hashmap))
        bin-list (first bin-and-labels)
        labelmap (nth 1 bin-and-labels))
    (jvm-assemble2 bin-list nil 0 labelmap)))

;; quick and dirty way to get argc from type string
;; should be something like this:
;; "\((I|L|F|D|C|(L.*;))*\)"
(defun count-semi (str idx acc)
  (cond
    (nil? (char-at str idx)) acc
    (equal? (char-at str idx) ")") acc
    (equal? (char-at str idx) "I") (count-semi str (inc idx) (inc acc))
    (equal? (char-at str idx) ";") (count-semi str (inc idx) (inc acc))
    t (count-semi str (inc idx) acc)))

(defun max-stack (jvm-asm mstack cstack mloc cloc lablist)
  (let (expr (first jvm-asm)
        _ (print "stack h: " cstack)
        _ (print "cmd:" expr)
        mstack (if (< mstack cstack) cstack mstack)
        mloc (if (< mloc cloc) cloc mloc))
    (if (nil? jvm-asm)
      (list mstack mloc lablist)
      (case (first expr)
        ;; add 1 to stack
        (:aload :aconst_null :iconst :dup :getstatic :ldc)
        (max-stack (rest jvm-asm) mstack (inc cstack) mloc cloc lablist)

        ;; remove 1 from stack
        (:areturn :pop :astore)
        (max-stack (rest jvm-asm) mstack (dec cstack) mloc cloc lablist)

        ;; remove 1 from stack, add labels
        (:ifnull :ifnonnull)
        (max-stack (rest jvm-asm) mstack (dec cstack) mloc cloc
                 (insert! lablist (nth 1 expr)
                          (list (dec cstack) cloc)))

        ;; add label, remove 1 from stack if not :start
        :goto
        (let (ncstack (if (equal? :start (second expr))
                        cstack
                        (dec cstack)))
          (max-stack (rest jvm-asm) mstack
                     ncstack
                     mloc cloc
                   (insert! lablist (nth 1 expr)
                            (list cstack cloc))))

        ;; ignore labels, return
        (:label :return)
        (max-stack (rest jvm-asm) mstack cstack mloc cloc lablist)

        ;; set based on local tag info
        (:local-count :let-pop)
        (max-stack (rest jvm-asm) mstack cstack mloc (nth 2 expr) lablist)

        ;; remove argc from stack, add 1 for result
        :invokestatic
        (max-stack (rest jvm-asm) mstack
                   (- cstack (dec (count-semi (nth 3 expr) 0 0)))
                   mloc cloc lablist)

        (print "max-stack can't handle " expr)))))

;; stack frame object entry of class Object
(defun objvar-info (pool)
  (list 0x07 (u2 (pool-get! pool (list :classref "java/lang/Object")))))

(defun sframe1 (offset l-count s-count last-local)
  (let (obj (objvar-info pool))
  (cond
    ; same frame
    (and (= last-local l-count) (= s-count 0) (< offset 64))
    (list offset)

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

(defun sframe-resolve1 (offsets stacklabs bytepos last-locals acc)
  (if offsets
    (let (lab (first (first offsets))
          off (second (first offsets))
          stack-and-local (get stacklabs lab)
          exists? (second stack-and-local)
          f-stack (first (first stack-and-local))
          f-local (second (first stack-and-local)))
      (if exists?
        (sframe-resolve1
          (rest offsets) stacklabs off f-local
          (cons (sframe1 (dec (- off bytepos)) f-local f-stack last-locals) acc))
        (sframe-resolve1 (rest offsets) stacklabs bytepos last-locals acc)))
    (reverse acc)))

(defun sframe-resolve (argc labels label-offsets)
  (let (offsets (keyvals label-offsets))
    (sframe-resolve1
      (msort (lambda (a b) (< (second a) (second b))) offsets)
      labels
      (- 1)
      argc
      nil)))

;; prepends start label to ir if there are tail recursive calls
(defun check-tco0 (in curr)
  (cond
    (nil? curr) in
    (equal? (first (first curr)) :tail-recur) (cons (list :label :start) in)
    t (check-tco0 in (rest curr))))

(defun compile0 (pool name args expr)
  (let (_ (print name)
        argc (length args)
        _ (print "raw ir===============")
        raw-ir (ir (symbol name) args expr)
        _ (map print raw-ir)
        _ (print "jvm asm===============")
        ; human readable jvm bytecode
        jvm-asm (->> (check-tco0 raw-ir raw-ir)
                     (map ir-to-jvm)
                     (semi-flatten))
        _ (map print jvm-asm)
        ; unresolved jump binary + label -> offset map
        _ (print "jvm assemble===============")
        bin-and-labels (jvm-assemble1 jvm-asm nil 0 (hashmap))
        _ (print "bin and labs:===============")
        _ (print bin-and-labels)
        ; binary with resolved jumps
        labelmap (nth 1 bin-and-labels)
        _ (print labelmap)
        _ (print "jvm bin")
        jvm-bin (jvm-assemble2 (first bin-and-labels) nil 0 labelmap)
        ;_ (print jvm-bin)
        bytecode (flatten jvm-bin) ; bytecode
        _ (print bytecode)
        code-size (length bytecode)
        ;_ (map print ir-list)
        stack-info (max-stack jvm-asm 0 0 0 argc (hashmap))
        _ (print "stack info:")
        _ (print stack-info)
        ;_ (print (nth 2 stack-info))
        stack-size (first stack-info)
        max-locals (second stack-info)
        ;_ (print max-locals)
        ;max-locals argc
        _ (print "stack frames")
        _ (print "argc" argc)
        _ (print (third stack-info))
        _ (if (get (third stack-info) :start nil)
            (insert! (third stack-info) :start (list 0 argc)))
        _ (print (third stack-info))
        stack-frames (sframe-resolve argc (nth 2 stack-info) labelmap)
        _ (print stack-frames)
        flat-stack-map (flatten stack-frames)
        stack-map-frames-size (length flat-stack-map)
        _ (print "end")
        _ (pprint "")
        )
    (list
      0x00 0x09 ; public static
      ; name of function
      (u2 (pool-get! pool (list :utf8 (string name))))
      ; type signature
      (u2 (pool-get! pool (list :utf8 (method-type argc))))
      0x00 0x01 ; attribute size of 1
      (u2 (pool-get! pool (list :utf8 "Code")))
      (if stack-frames
        (u4 (+ 12 code-size 8 stack-map-frames-size))
        (u4 (+ 12 code-size)))
      (u2 stack-size)
      (u2 max-locals)
      (u4 code-size)
      bytecode
      0x00 0x00 ; 0 exceptions
      (if stack-frames
        (list 0x00 0x01 ; one attribute (StackMapTable)
              (u2 (pool-get! pool (list :utf8 "StackMapTable")))
              ; length in bytes = size(number of frames) + length of binary
              (u4 (+ 2 stack-map-frames-size))
              (u2 (length stack-frames)) ; number of frames
              stack-frames)
        ; no attributes if StackMapTable is empty
        (list 0x00 0x00)))))

(defun compile-special (pool name args signature expr)
  (let (_ (print name)
        argc (length args)
        _ (print "raw ir===============")
        _ (print expr)
        ;raw-ir (ir (symbol name) args expr)
        raw-ir (ir t args expr)
        _ (map print raw-ir)
        _ (print "jvm asm===============")
        ; human readable jvm bytecode
        jvm-asm (->> (check-tco0 raw-ir raw-ir)
                     (map ir-to-jvm)
                     (semi-flatten)
                     (reverse)
                     (rest)
                     (cons (list :return))
                     (reverse)
                     )
        _ (map print jvm-asm)
        ; unresolved jump binary + label -> offset map
        _ (print "jvm assemble===============")
        bin-and-labels (jvm-assemble1 jvm-asm nil 0 (hashmap))
        _ (print "bin and labs:===============")
        _ (print bin-and-labels)
        ; binary with resolved jumps
        labelmap (nth 1 bin-and-labels)
        _ (print labelmap)
        _ (print "jvm bin")
        jvm-bin (jvm-assemble2 (first bin-and-labels) nil 0 labelmap)
        ;_ (print jvm-bin)
        bytecode (flatten jvm-bin) ; bytecode
        _ (print bytecode)
        code-size (length bytecode)
        ;_ (map print ir-list)
        stack-info (max-stack jvm-asm 0 0 0 argc (hashmap))
        _ (print "stack info:")
        _ (print stack-info)
        ;_ (print (nth 2 stack-info))
        stack-size (first stack-info)
        max-locals (second stack-info)
        ;_ (print max-locals)
        ;max-locals argc
        _ (print "stack frames")
        _ (print "argc" argc)
        _ (print (third stack-info))
        _ (if (get (third stack-info) :start nil)
            (insert! (third stack-info) :start (list 0 argc)))
        _ (print (third stack-info))
        stack-frames (sframe-resolve argc (nth 2 stack-info) labelmap)
        _ (print stack-frames)
        flat-stack-map (flatten stack-frames)
        stack-map-frames-size (length flat-stack-map)
        _ (print "end")
        _ (pprint "")
        )
    (list
      0x00 0x09 ; public static
      ; name of function
      (u2 (pool-get! pool (list :utf8 (string name))))
      ; type signature
      (u2 (pool-get! pool (list :utf8 signature)))
      0x00 0x01 ; attribute size of 1
      (u2 (pool-get! pool (list :utf8 "Code")))
      (if stack-frames
        (u4 (+ 12 code-size 8 stack-map-frames-size))
        (u4 (+ 12 code-size)))
      (u2 stack-size)
      (u2 max-locals)
      (u4 code-size)
      bytecode
      0x00 0x00 ; 0 exceptions
      (if stack-frames
        (list 0x00 0x01 ; one attribute (StackMapTable)
              (u2 (pool-get! pool (list :utf8 "StackMapTable")))
              ; length in bytes = size(number of frames) + length of binary
              (u4 (+ 2 stack-map-frames-size))
              (u2 (length stack-frames)) ; number of frames
              stack-frames)
        ; no attributes if StackMapTable is empty
        (list 0x00 0x00)))))


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
           ;(msort (lambda (a b) (- (second a) (second b))))
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

(defun compile1 (name methods)
  (let (pool (hashmap)
    bin-funs (map (lambda (x) (compile1 pool (first x) (second x) (nth 2 x)))
           methods))
    (class-headers pool name "java/lang/Object" bin-funs)))

(include "read.lisp")

(defun compile2 (path classname)
  (let (exprs (read-all path)
        _ (print "done reading")
        funs  (filter (lambda (x) (equal? (first x) (quote defun)))
                      exprs)
        defs (filter (lambda (x) (equal? (first x) (quote def)))
                      exprs)
        cpool (hashmap)
        _ (map (lambda (x)
           (insert-method
             (string (second x))
             classname
             (string (second x))
             (length (third x))))
         funs)
        cinitbin (compile-special cpool "<clinit>" (quote ())
                           "()V"
                           (cons (quote progn) defs))
        mainbin (compile-special cpool "main" (quote (0))
                                 "([Ljava/lang/String;)V"
                                 (quote (main)))
        ;x (throw asdf)
        )
    (progn
    (map print funs)
    
    (map print (keyvals method-list))
    ;(print (cons (quote progn) defs))
    (write-bytes "MyClass.class"
               (flatten
    (class-headers cpool classname "java/lang/Object"
                   (cons cinitbin mainbin
    (map (lambda (x)
         (compile0 cpool (second x) (third x) (nth 3 x)))
       funs))))))
  ))

;; include
    ;; recursively compile includes
;; defmacro
;; defun

;; everything else is compiled into main
