(defun to-u2 (x)
  (let (len-lo (% x 0xFF)
        len-hi (// x 0xFF))
    (list len-hi len-lo)))

(defun utf8-info (str)
  ; utf8 tag is 0x01
  (concat (cons 0x01
                (to-u2 (length str)))
          (map char-int (to-chars str))))

(defun classref-info (num)
  (cons 0x07 (to-u2 num)))

(defun string-info (num)
  (cons 0x08 (to-u2 num)))

(defun fieldref-info (class nametype)
  (cons 0x09
        (concat (to-u2 class)
                (to-u2 nametype))))

(defun methodref-info (class nametype)
  (cons 0x0A
        (concat (to-u2 class)
                (to-u2 nametype))))

(defun nametyperef-info (name descriptor)
  (cons 0x0C
        (concat (to-u2 name)
                (to-u2 descriptor))))

(def constant-pool
     (list
       (utf8-info "Hello")                  ; #1
       (classref-info 1)                    ; #2  Class #1 Hello
       (utf8-info "java/lang/Object")       ; #3
       (classref-info 3)                    ; #4  Class #3 java.lang.Object
       (utf8-info "java/lang/System")       ; #5
       (classref-info 5)                    ; #6  Class #5 java.lang.System
       (utf8-info "java/io/PrintStream")    ; #7
       (classref-info 7)                    ; #8  Class #7 java.io.PrintStream
       (utf8-info "out")                    ; #9
       (utf8-info "Ljava/io/PrintStream;")  ; #10
       (nametyperef-info 9 10)              ; #11 member out of type PrintStream
       (fieldref-info 6 11)                 ; #12 System.out
       (utf8-info "println")                ; #13
       (utf8-info "(Ljava/lang/String;)V")  ; #14
       (nametyperef-info 13 14)             ; #15 member println of type void(String)
       (methodref-info 8 15)                ; #16 void PrintStream.println(String)
       (utf8-info "Code")                   ; #17
       (utf8-info "main")                   ; #18
       (utf8-info "([Ljava/lang/String;)V") ; #19
       (utf8-info "Hello World!")           ; #20
       (string-info 20)))                   ; #21 String "Hello World" (#20)

(def hello-method
     (list 0x00 0x09            ; public static
           0x00 0x12            ; main (#18)
           0x00 0x13            ; void (String[])
           0x00 0x01            ; attribute size = 1
           0x00 0x11            ; attribute: Code (#17)
           0x00 0x00 0x00 0x15  ; code attribute size = 21
           0x00 0x02            ; max stack size = 2
           0x00 0x01            ; max local var size = 1
           0x00 0x00 0x00 0x09  ; 9 bytes of code
           0xB2 0x00 0x0c       ; getstatic System.out (#12)
           0x12 0x15            ; ldc "Hello World" (#21)
           0xB6 0x00 0x10       ; invokevirtual PrintStream.println(String) (#16)
           0xB1                 ; return
           0x00 0x00            ; exception table size = 0
           0x00 0x00))          ; attribute size = 0

(def my-class
     (reduce 
       concat
       (list 
         (list 0xCA 0xFE 0xBA 0xBE  ; CAFE BABE magic number
               0x00 0x00 0x00 0x37) ; 0 0 . 0 55 => Java version 55.0 (Java 11)
         (to-u2 (inc (length constant-pool)))
         (reduce concat constant-pool)
         (list 0x00 0x21) ; class access flags: extendable and public
         (list 0x00 0x02) ; class Hello (#2)
         (list 0x00 0x04) ; extends java.lang.Object (#4)
         (list 0x00 0x00) ; zero interfaces
         (list 0x00 0x00) ; zero fields
         (list 0x00 0x01) ; 1 method
         hello-method
         (list 0x00 0x00)))) ; zero attributes

(write-bytes "Hello.class" my-class) 
