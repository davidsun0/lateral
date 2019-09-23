(include "compiler.lisp")

(defun to-u1 (x)
  (if (< x 0xFF)
    (list x)
    "int too large for 1 byte"))

(defun to-u2 (x)
  (if (or (< x 0xFFFF) (= x 0xFFFF))
    (list (bit-and (// x 0xFF) 0x100)
          (bit-and x 0xFF))
    "int too large for 2 bytes"))

(defun to-u4 (x)
  (concat (to-u2 (// x 0x10000))
          (to-u2 (bit-and x 0xFFFF))))

(defun const-to-bin (const-list)
  (cond
    (equal? (car const-list) :utf8)
    (concat (cons 0x01 (to-u2 (length (nth 1 const-list))))
            (map char-int (to-chars (nth 1 const-list))))

    (equal? (car const-list) :classref)
    (cons 0x07 (to-u2 (nth 1 const-list)))

    (equal? (car const-list) :string)
    (cons 0x08 (to-u2 (nth 1 const-list)))

    (equal? (car const-list) :fieldref)
    (cons 0x09
          (concat (to-u2 (nth 1 const-list))
                  (to-u2 (nth 2 const-list))))

    (equal? (car const-list) :methodref)
    (cons 0x0A 
          (concat (to-u2 (nth 1 const-list))
                  (to-u2 (nth 2 const-list))))

    (equal? (car const-list) :nametyperef)
    (cons 0x0C
          (concat (to-u2 (nth 1 const-list))
                  (to-u2 (nth 2 const-list))))

    t (list :unknown const-list)))

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

                 t (list :unknown parent)
                 )
               pool-list))
        (hashmap-set! poolmap parent pool-count)
        (def pool-count (inc pool-count))
        (dec pool-count)
        ))))

(def pool (make-hashmap 32))
(def pool-count 1)
(def pool-list nil)

(let (presolve (lambda (k v) (pool-resolve pool k)))
 (maphash presolve pool))
(def pool-list (reverse! pool-list))
(def bin-pool (reduce concat (map const-to-bin pool-list)))

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
                                             "Ljava/lang/Boolean;"))
      
      (equal? cmd :jump-if-nil)
      (cons :ifnull (cdr expr))
      t expr
      )))

;; converts a single item of the form (:jvmcode args) into a list of bytes
(defun jvm-assemble0 (in)
    (let (cmd (car in))
      (cond
        (equal? cmd :aconst_null)
        (list 0x01)

        (equal? cmd :aload)
        (cons 0x19 (to-u1 (nth 1 expr)))

        (equal? cmd :ldc)
        (cons 0x12 (to-u1 (pool-get pool (nth 1 expr))))

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
        t expr
        )))

;; resolves labels and compiles to byte lists whenever possible
(defun jvm-assemble1 (in acc offset labelmap)
  (if in
    (progn ; (print (car in))
    (let (expr (car in)
          binexpr (jvm-assemble0 expr)
          binexprlen (cond
                   (int? (car binexpr)) (length binexpr)
                   
                   (equal? (car expr) :ifnull) 3 ; 2 bytes for offset
                   (equal? (car expr) :goto) 3 ; 2 bytes for offset
                   (equal? (car expr) :label) 0 ; no equivalent of label
                   )
          newlabels (if (equal? (car expr) :label)
                      (hashmap-set! labelmap (nth 1 expr) offset)
                      labelmap))
      ; add to acc
      ; calculate offset
      ; add to labelmap
      (jvm-assemble1 (cdr in)
                     (if (= binexprlen 0) acc (cons binexpr acc))
                     (+ offset binexprlen)
                     newlabels)))
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

(defun pool-get (constpool expr)
  (progn
    (if (nil? (nth 1 (hashmap-get constpool expr)))
      (pool-add constpool expr))
    (pool-resolve constpool expr)))

(defun compile (name args expr)
  (let (ir-list (append (ir0 expr nil) (list :return))
        max-stack (max-stack0 ir-list 0 0 nil)
        max-locals (length args)
        bytecode (flatten
                   (jvm-assemble
                   (map ir-to-jvm
                        (resolve-syms ir-list args))))
        code-size (length bytecode)
        code-attribute-size (+ 12 code-size))
    (list
      0x00 0x09 ; public static
      (to-u2 (pool-get pool (list :utf8 name)))
      (to-u2 (pool-get pool (list :utf8 "(Ljava/lang/Object;)Ljava/lang/Object;")))
      0x00 0x01 ; attribute size of 1
      (to-u2 (pool-get pool (list :utf8 "Code")))
      (to-u4 code-attribute-size)
      (to-u2 max-stack)
      (to-u2 max-locals)
      (to-u4 code-size)
      bytecode
      0x00 0x00 0x00 0x00 ; 0 exceptions, 0 attributes
      )))

(def identity-byte (compile "identity" (quote (a)) (quote a)))
; (def nil?-byte (compile "nil?" (quote (p)) (quote (if p nil t))))
(print identity-byte)
(def not-ir (append (ir0 (quote (if p nil t)) nil) (list :return)))
(def not-jvm (map ir-to-jvm (resolve-syms not-ir (quote (p)))))
; (print (jvm-assemble1 not-jvm nil 0 (make-hashmap 8)))
; (print (jvm-assemble not-jvm))
(def not-byte (compile "not" (quote (p)) (quote (if p nil t))))
(print not-byte)

; (def main-bytecode
;       (let (bytecode (quote (
;           (:getstatic "java/lang/System" "out" "Ljava/io/PrintStream;")
;           (:ldc (:string "Hello World!"))
;           (:invokestatic "Lateral" "identity" "(Ljava/lang/Object;)Ljava/lang/Object;")
;           (:checkcast "java/lang/String")
;           (:invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
;           (:return)))
;           binary-code (flatten (jvm-assemble bytecode))
;           code-size (length binary-code))
;     (list
;       0x00 0x09
;       (to-u2 (pool-get pool (list :utf8 "main")))
;       (to-u2 (pool-get pool (list :utf8 "([Ljava/lang/String;)V")))
;       0x00 0x01
;       (to-u2 (pool-get pool (list :utf8 "Code")))
;       (to-u4 (+ 12 code-size))
;       (to-u2 2) ;; MAX STACK SIZE - MUST BE HAND UPDATED
;       (to-u2 1) ;; MAX LOCAL SIZE - MUST BE HAND UPDATED
;       (to-u4 code-size)
;       binary-code
;       0x00 0x00 0x00 0x00)))

(defun class-headers (name parent methods)
  (progn
    ;; do not delete; need to add to pool
    (pool-get pool (list :classref name))
    (pool-get pool (list :classref parent))
  (list
    0xCA 0xFE 0xBA 0xBE ; java magic number
    0x00 0x00 0x00 0x37 ; java version 55.0 (Java 11)
    (to-u2 (inc (length pool-list)))
    (flatten (map const-to-bin (reverse pool-list)))
    0x00 0x21 ; extendable (not final) and public
    (to-u2 (pool-get pool (list :classref name)))
    (to-u2 (pool-get pool (list :classref parent)))
    0x00 0x00 ; zero interfaces
    0x00 0x00 ; zero fields
    (to-u2 (length methods))
    methods
    0x00 0x00 ; zero attributes
    )))

; (print (flatten (compile "identity" (quote (a)) (quote a))))
(print "===")
(def class-bin (class-headers "Lateral"
                      "java/lang/Object"
                      (list identity-byte ;main-bytecode
                            not-byte
                            )))
; (print class-bin)
; (print (flatten class-bin))
(print "===")
(write-bytes "Lateral.class" (flatten class-bin))
(print "===")
; (map print pool-list)
