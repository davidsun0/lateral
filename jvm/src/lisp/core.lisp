;;; ===================================
;;;  CORE UTILITY
;;; ===================================

(defun load (file)
  (asm-quote (asm-unquote file)
             (:checkcast "java/lang/String")
             (:invokestatic "lateral/lang/Compiler"
                            "load"
                            "(Ljava/lang/String;)Ljava/lang/Object;")))

(def t (asm-quote (:getstatic "java/lang/Boolean" "TRUE" "Ljava/lang/Boolean;")))
(def nil (asm-quote :aconst_null))

(defun nil? (a)
  (if a nil t))

(def not nil?)

(defun print (x)
  (asm-quote (asm-unquote x)
             :dup
             (:getstatic "java/lang/System" "out" "Ljava/io/PrintStream;")
             :swap
             (:invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/Object;)V")))

(defun + (a b)
  (asm-quote (asm-unquote a)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (asm-unquote b)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             :iadd
             (:invokestatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;")))

(defun > (a b)
  (asm-quote (asm-unquote a)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (asm-unquote b)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (:if_icmple falsebranch)
             (asm-unquote t)
             :areturn
             (:label falsebranch)
             (asm-unquote nil)
             :areturn))

(defun inc (n)
  (asm-quote (asm-unquote n)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (:iconst 1)
             :iadd
             (:invokestatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;")))


(defun dec (n)
  (asm-quote (asm-unquote n)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (:iconst -1)
             :iadd
             (:invokestatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;")))

(defun even? (n)
  (asm-quote (asm-unquote n)
             (:checkcast "java/lang/Integer")
             (:invokevirtual "java/lang/Integer" "intValue" "()I")
             (:iconst 1)
             :iand
             (:ifne falsebranch)
             (asm-unquote t)
             :areturn
             (:label falsebranch)
             (asm-unquote nil)
             :areturn))

(defun empty? (x)
  ;; Sequences should never be nil, only empty
  (asm-quote (asm-unquote x)
             (:instanceof "lateral/lang/Sequence")
             (:ifeq falsebranch)
             (asm-unquote x)
             (:checkcast "lateral/lang/Sequence")
             (:invokevirtual "lateral/lang/Sequence"
                             "isEmpty"
                             "()Z")
             (:ifeq falsebranch)
             (asm-unquote t)
             :areturn
             (:label falsebranch)
             (asm-unquote nil)
             :areturn))

(defun cons (x lst)
  (asm-quote (asm-unquote x)
             (asm-unquote lst)
             (:checkcast "lateral/lang/Sequence")
             (:invokestatic "lateral/lang/Sequence"
                            "cons"
                            "(Ljava/lang/Object;Llateral/lang/Sequence;)Llateral/lang/Sequence;")))

(defun first (lst)
  (asm-quote (asm-unquote lst)
             (:checkcast "lateral/lang/Sequence")
             (:invokevirtual "lateral/lang/Sequence"
                             "first"
                             "()Ljava/lang/Object;")))

(defun second (lst)
  (asm-quote (asm-unquote lst)
             (:checkcast "lateral/lang/Sequence")
             (:invokevirtual "lateral/lang/Sequence"
                             "second"
                             "()Ljava/lang/Object;")))

(defun rest (lst)
  (asm-quote (asm-unquote lst)
             (:checkcast "lateral/lang/Sequence")
             (:invokevirtual "lateral/lang/Sequence"
                             "rest"
                             "()Llateral/lang/Sequence;")))

(defun list? (obj)
  (asm-quote (asm-unquote obj)
             (:instanceof "lateral/lang/Sequence")
             (:ifeq falsebranch)
             (asm-unquote t)
             :areturn
             (:label falsebranch)
             (asm-unquote nil)
             :areturn))

(defun reverse (lst)
  ((function (lst acc)
     (if (empty? lst)
       acc
       (recur (rest lst)
              (cons (first lst) acc))))
   lst '()))

(defun length (lst)
  ((function (lst acc)
     (if (empty? lst)
       acc
       (recur (rest lst) (inc acc))))
   lst 0))

;;; ===================================
;;;  MACRO ESSENTIALS
;;; ===================================

;; list is built in to the compiler; this definition is for higher order programming
(defun list (:rest lst)
  lst)

(defun gensym
   ()       (gensym "gensym")
   (prefix) (asm-quote (asm-unquote prefix)
                       (:checkcast "java/lang/String")
                       (:invokestatic "lateral/lang/Symbol"
                                      "gensym"
                                      "(Ljava/lang/String;)Llateral/lang/Symbol;")))

;; for quasiquote, calls Sequence.concat in Sequence.java
(defun concat (:rest lst)
  (asm-quote (asm-unquote lst)
             (:checkcast "lateral/lang/Sequence")
             (:invokestatic "lateral/lang/Sequence"
                            "concat"
                            "(Llateral/lang/Sequence;)Llateral/lang/Sequence;")))

(defun macroexpand (form)
  (asm-quote (asm-unquote form)
             (:invokestatic "lateral/lang/Compiler"
                            "macroExpand"
                            "(Ljava/lang/Object;)Ljava/lang/Object;")))

;;; ===================================
;;;  CONTROL FLOW
;;; ===================================
;;; NOTE: Macros MUST come before any usages.
;;; Otherwise the symbol will be assumed to be a function

(defmacro or (:rest lst)
  (if (empty? lst)
    'nil
    (let (rev (reverse lst))
      ((function (lst acc)
         (if (empty? lst)
           acc
           (recur (rest lst)
                  (list 'if (first lst) 't acc))))
       (rest rev)
       (first rev)))))

(defmacro and (:rest lst)
  (if (empty? lst)
    't
    (let (rev (reverse lst))
      ((function (lst acc)
         (if (empty? lst)
           acc
           (recur (rest lst)
                  (list 'if (first lst) acc 'nil))))
       (rest rev)
       (first rev)))))

(defmacro cond (:rest binds)
  (let (rbinds      (reverse binds)
        evenlength? (even? (length binds)))
  ((function (lst acc)
     (if (empty? lst)
       acc
       (recur (rest (rest lst))
              (list 'if
                    (second lst)
                    (first lst)
                    acc))))
   (if evenlength? rbinds (rest rbinds))
   (if evenlength? 'nil (first rbinds)))))

(defun case0 (sym binds acc)
  (cond
    (empty? binds)        acc
    (empty? (rest binds)) (cons (first binds) (cons 't acc))
    t                     (recur sym
                                 (rest (rest binds))
                                 (cons (second binds)
                                       (cons `(eq? ,sym ,(first binds))
                                             acc)))))

(defmacro case (val :rest binds)
  (let (sym (gensym "case"))
    `(let (,sym ,val)
      (cond ,@(reverse (case0 sym binds '()))))))

;;; Clojure's thread first macro
(defmacro -> (:rest forms)
  ((function (lst acc)
     (if (empty? lst)
       acc
       (recur (rest lst)
              `(,(first (first lst))
                ,acc
                ,@(rest (first lst))))))
   (rest forms)
   (first forms)))

;;; Clojure's thread last macro
(defmacro ->> (:rest forms)
  ((function (lst acc)
     (if (empty? lst)
       acc
       (recur (rest lst)
              (concat (first lst)
                      (list acc)))))
   (rest forms)
   (first forms)))


;;; ===================================
;;;  LIST MANIPULATION
;;; ===================================

(defun eq? (a b)
  (if (and (nil? a) (nil? b))
    t
    (asm-quote (asm-unquote (if a a b))
               (asm-unquote (if a b a))
               ; args are swapped so a is never nil
               ; this way Object.equals never throws NullPointerException
               (:invokevirtual "java/lang/Object"
                               "equals"
                               "(Ljava/lang/Object;)Z")
               (:ifeq falsebranch)
               (asm-unquote t)
               :areturn
               (:label falsebranch)
               ; implicit :areturn in asm-quote
               (asm-unquote nil))))

(defun zip (left right)
  ((function (l r acc)
     (if (and (empty? l)
              (empty? r))
       (reverse acc)
       (recur (rest l)
              (rest r)
              (cons (list (first l)
                          (first r))
                    acc))))
   left right '()))

(defun range
  (low hi)
  ((function (low hi acc)
     (if (> hi low)
       (recur low (dec hi) (cons hi acc))
       acc))
   (dec low) (dec hi) '())

  (hi)
  (range 0 hi))

;;; pairs up elements of a list
;; (a b c d) -> ((a b) (c d))
;; odd inputs get paired with nil
;; (a b c d e) -> ((a b) (c d) (e nil))
;; TODO: unsafe pairs that throws for odd lists?
(defun pairs (lst)
  ((function (lst acc)
     (if (empty? lst)
       (reverse acc)
       (recur (rest (rest lst))
              (cons (list (first lst)
                          (second lst))
                    acc))))
   lst '()))


;;; Measures wall time execution of clock in ms
(defmacro time (form)
  `(asm-quote
    (:invokestatic "java/lang/System" "currentTimeMillis" "()J")
    (asm-unquote ,form)
    :dup_x2
    :pop
    (:invokestatic "java/lang/System" "currentTimeMillis" "()J")
    :lsub
    :lneg
    ;; poor man's System.out.printf("Evaluation took %lms of real time%n");
    (:getstatic "java/lang/System" "out" "Ljava/io/PrintStream;")
    :dup
    (:ldc "Evaluation took ") ; Stack: Object, long, out, out, String
    (:invokevirtual "java/io/PrintStream" "print" "(Ljava/lang/String;)V") ; Stack: Object, long, out
    :dup_x2
    :dup_x2
    :pop ; Stack: Object, out, out, long
    (:invokevirtual "java/io/PrintStream" "print" "(J)V") ; Stack: Object, out
    (:ldc "ms of real time") ; Stack: Object, out, String
    ;; at the end, asm-quote implicitly returns the top object on the stack - i.e. the result of form
    (:invokevirtual "java/io/PrintStream" "println" "(Ljava/lang/String;)V")))

;;; HASHMAP

(defun hashmap (:rest kvlist)
  (let (newmap (asm-quote (:new "java/util/HashMap")
                          :dup
                          (:invokespecial "java/util/HashMap" "<init>" "()V"))
        _      (map (function (kvpair)
                      (assoc! newmap (first kvpair) (second kvpair)))
                    (pairs kvlist)))
    newmap))

(defun assoc! (hmap key value)
  (asm-quote (asm-unquote hmap)
             (:checkcast "java/util/HashMap")
             :dup
             (asm-unquote key)
             (asm-unquote value)
             (:invokevirtual "java/util/HashMap"
                             "put"
                             "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
             :pop))

(defun assoc (hmap key value)
  (let (clone (asm-quote (asm-unquote hmap)
                         (:checkcast "java/util/HashMap")
                         (:invokevirtual "java/util/HashMap"
                                         "clone"
                                         "()Ljava/lang/Object;")))
    (assoc! clone key value)))

;; TODO: desoc

(defun get (hmap key)
  (asm-quote (asm-unquote hmap)
             (:checkcast "java/util/HashMap")
             (asm-unquote key)
             (:invokevirtual "java/util/HashMap"
                             "get"
                             "(Ljava/lang/Object;)Ljava/lang/Object;")))

(defun contains? (hashmap key)
  nil)

(defun vector (:rest elements)
  (let (newvec (asm-quote (:new "java/util/ArrayList")
                          :dup
                          (:invokespecial "java/util/ArrayList" "<init>" "()V"))
        _      (map (function (x) (append! newvec x))
                    elements))
    newvec))

(defun append! (vec value)
  (asm-quote (asm-unquote vec)
             (:checkcast "java/util/ArrayList")
             :dup
             (asm-unquote value)
             (:invokevirtual "java/util/ArrayList"
                             "add"
                             "(Ljava/lang/Object;)Z")
             :pop))

(defun append (vec value)
  (let (clone (asm-quote (asm-unquote vec)
                         (:checkcast "java/util/ArrayList")
                         (:invokevirtual "java/util/ArrayList"
                                         "clone"
                                         "()Ljava/lang/Object;")))
    (append! clone value)))

;;; ===================================
;;;  HIGHER ORDER PROGRAMMING
;;; ===================================

(defun apply (fun args)
  (asm-quote (asm-unquote fun)
             (:checkcast "lateral/lang/Function")
             (asm-unquote args)
             (:checkcast "lateral/lang/Sequence")
             (:invokestatic "lateral/lang/Function"
                            "apply"
                            "(Llateral/lang/Function;Llateral/lang/Sequence;)Ljava/lang/Object;")))

(defun map (f lst)
  ((function (lst acc)
     (if (empty? lst)
       (asm-quote (asm-unquote acc)
                  (:checkcast "java/util/ArrayList")
                  (:invokevirtual "java/util/ArrayList"
                                  "toArray"
                                  "()[Ljava/lang/Object;")
                  (:invokestatic  "lateral/lang/Sequence"
                                  "makeList"
                                  "([Ljava/lang/Object;)Llateral/lang/Sequence;"))
       (recur (rest lst)
              (append! acc (f (first lst))))))
   ;; manual creation of empty ArrayList because (vector) uses map internally
   lst (asm-quote (:new "java/util/ArrayList")
                  :dup
                  (:invokespecial "java/util/ArrayList" "<init>" "()V"))))
       
(defun filter (f lst)
  ((function (lst acc)
     (cond
       (empty? lst)
       (asm-quote (asm-unquote acc)
                  (:checkcast "java/util/ArrayList")
                  (:invokevirtual "java/util/ArrayList"
                                  "toArray"
                                  "()[Ljava/lang/Object;")
                  (:invokestatic  "lateral/lang/Sequence"
                                  "makeList"
                                  "([Ljava/lang/Object;)Llateral/lang/Sequence;"))

       (f (first lst))
       (recur (rest lst)
              (append! acc (first lst)))

       (recur (rest lst) acc)))
   lst (vector)))
