(defun to-u1 (x)
  (if (< x 0xFF)
    (list x)
    "int too large for 1 byte"))

(defun to-u2 (x)
  (if (< x 0xFFFF)
    (concat (to-u1 (// x 0xFF))
            (to-u1 (% x 0xFF)))
    "int too large for 2 bytes"))

(defun to-u4 (x)
  (concat (to-u2 (// x 0xFFFF))
          (to-u2 (% x 0xFFFF))))

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

      t (hashmap-set poolmap (cons :unknown attribs))
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

(pool-add pool (list :classref "Hello"))
(pool-add pool (list :classref "java/lang/Object"))
(pool-add pool (list :utf8 "Code"))
(pool-add pool (list :utf8 "main"))
(pool-add pool (list :utf8 "([Ljava/lang/String;)V"))
(pool-add pool (list :string "Hello World!"))

(pool-add pool
          (list :methodref
                (list :classref "java/io/PrintStream")
                (list :nametyperef "println" "(Ljava/lang/String;)V")))
(pool-add pool
          (list :fieldref
                (list :classref "java/lang/System")
                (list :nametyperef "out" "Ljava/io/PrintStream;")))

(let (presolve (lambda (k v) (pool-resolve pool k)))
 (maphash presolve pool))
(def pool-list (reverse! pool-list))

(def hello-method2
     (list 0x00 0x09            ; public static
           ; 0x00 0x12          ; main (#18)
           (to-u2 (car (hashmap-get pool (list :utf8 "main"))))
           ; 0x00 0x13          ; void (String[]) (#19)
           (to-u2 (car (hashmap-get pool (list :utf8 "([Ljava/lang/String;)V"))))
           0x00 0x01            ; attribute size = 1
           ; 0x00 0x11          ; attribute: Code (#17)
           (to-u2 (car (hashmap-get pool (list :utf8 "Code"))))
           0x00 0x00 0x00 0x15  ; code attribute size = 21
           0x00 0x02            ; max stack size = 2
           0x00 0x01            ; max local var size = 1
           0x00 0x00 0x00 0x09  ; 9 bytes of code
           0xB2                 ; getstatic
           (to-u2 (car (hashmap-get pool (list :fieldref
                                               (list :classref "java/lang/System")
                                               (list :nametyperef
                                                     "out"
                                                     "Ljava/io/PrintStream;")))))
           0x12                 ; ldc
           (to-u1 (car (hashmap-get pool (list :string "Hello World!"))))
           0xB6                 ; invokevirtual
           (to-u2 (car (hashmap-get pool (list :methodref
                                               (list :classref "java/io/PrintStream")
                                               (list :nametyperef
                                                     "println"
                                                     "(Ljava/lang/String;)V")))))
           0xB1                 ; return
           0x00 0x00            ; exception table size = 0
           0x00 0x00))          ; attribute size = 0

(def hello-method3
     (list
       (list :public :static "main" "([Ljava/lang/String;)V")
       (list :max-stack 2 :max-locals 1)
       (list :getstatic "java/lang/System/out" "Ljava/io/PrintStream;")
       (list :ldc "Hello World!")
       (list :invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
       (list :return)))

(map print pool-list)
(def bin-pool (reduce concat (map const-to-bin pool-list)))

(list :class "Hello" :extends "java/lang/Object")

(def my-class
     (reduce 
       concat
       (list 
         (list 0xCA 0xFE 0xBA 0xBE  ; CAFE BABE magic number
               0x00 0x00 0x00 0x37) ; 0 0 . 0 55 => Java version 55.0 (Java 11)
         (to-u2 (inc (length pool-list)))
         bin-pool
         (list 0x00 0x21) ; class access flags: extendable and public
         ; (list 0x00 0x02) ; class Hello (#2)
         (to-u2 (car (hashmap-get pool (list :classref "Hello"))))
         ; (list 0x00 0x04) ; extends java.lang.Object (#4)
         (to-u2 (car (hashmap-get pool (list :classref "java/lang/Object"))))
         (list 0x00 0x00) ; zero interfaces
         (list 0x00 0x00) ; zero fields
         (list 0x00 0x01) ; 1 method
         (flatten hello-method2)
         (list 0x00 0x00)
         ))) ; zero attributes

; (write-bytes "Hello.class" my-class) 

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

(defun compile (name args expr)
  (let (ir-list (append (ir0 expr nil) (list :return))
        max-stack-size (max-stack0 ir-list 0 0 nil)
        bytecode (map ir-to-jvm (resolve-syms ir-list args)))
    (progn
      (print max-stack-size)
      (map print bytecode))))

(compile "identity" (quote (a)) (quote a))
