(defpackage :lateral
  (:use :cl))
(in-package :lateral)

(defparameter *function-table* nil)

(defclass envir ()
  ((locals
    :initform nil
    :accessor locals)
   (type
    :initarg :type)
   (outer
    :initform nil)))

(defmacro emit (str &rest rest)
  `(format output ,(concatenate 'string str "~%") ,@rest))

(defun compile-expr (&rest exprs)
  (setf *function-table* nil)
  (with-open-file (output "output.s"
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format output ".data~%dec:~%.string \"%d\\n\"~%") ; for print function
    (emit ".text")
    (dolist (expr exprs) (compile-form expr nil output))))

(defun compile-un (fn output)
  "Compiles built-in functions that take one argument"
  (progn
    (case fn
      (print (progn
	       (emit "push {r0}")
	       (emit "ldr r1, [r0, #4]")
	       (emit "ldr r0, =dec")
	       (emit "bl printf")
	       (emit "pop {r0}")))
      (t (error "Can't compile unary function ~A" fn)))))

(defun compile-bi (fn output)
  "Compiles built-in functions that take two arguments"
  (progn
    (emit "pop {r0, r1}~%")
    (emit "ldr r0, [r0, #4]")
    (emit "ldr r1, [r1, #4]")
    (case fn
      (+ (emit "add r0, r1, r0"))
      (- (emit "sub r0, r1, r0"))
      (* (emit "mul r0, r1, r0"))
      (> (progn
	   (emit "cmp r1, r0")
	   (emit "movgt r0, #1")
	   (emit "movle r0, #0")))
      (t (error "Can't compile binary function ~A" fn)))
    (emit "push {r0}")
    (emit "ldr r0, =8")
    (emit "bl malloc")
    (emit "pop {r1}")
    (emit "str r1, [r0, #4]")
    (emit "ldr r1, =1") ; set object type to int
    (emit "str r1, [r0]")))

(defun compile-fn (expr output)
  "Compiles function call expressions"
  (let ((paramc (cdr (assoc (car expr) *function-table*))))
    (if paramc
	(if (= paramc (length (cdr expr)))
	    (progn
	      (case paramc
		(1 (emit "pop {r0}"))
		(2 (emit "pop {r0, r1}"))
		(3 (emit "pop {r0, r1, r2}"))
		(4 (emit "pop {r0, r1, r2, r3}"))
		(t (error "Can't call a function with ~A arguments" paramc)))
	      (emit "bl ~(~A~)" (car expr)))
	    (error "Function ~A expected ~A arguments, but got ~A"
		   (car expr) paramc (length (cdr expr))))
	(case (length (cdr expr))
	  ;; compiler built in functions
	  (1 (compile-un (car expr) output))
	  (2 (compile-bi (car expr) output))
	  (t (error "Can't compile function ~A with ~A arguments"
		    (car expr) (length (cdr expr))))))))

(defun compile-form (obj envir output)
  "Compiles a general lisp form"
  (cond
    ((numberp obj)
     (progn
       (emit "ldr r0, =8")
       (emit "bl malloc")
       (emit "ldr r1, =1")
       (emit "str r1, [r0]")
       (emit "ldr r1, =~A" obj)
       (emit "str r1, [r0, #4]")))
    ((symbolp obj)
     (let ((local-index (position obj envir)))
       (if local-index
	   (emit "ldr r0, [fp, #~A] @ argument ~A" (- (* 4 local-index) 16) local-index)
	   (error "Can't find variable ~A in local environment: ~A" obj envir))))
    ((listp obj)
     (case (car obj)
       (defun
	(let ((name   (second obj))
	      (params (third obj))
	      (body   (fourth obj)))
	  (assert (= 4 (length obj)))
	  (assert (<= (length params) 4)) ; can only hand args in r0 - r3 right now
	  (setf *function-table* (acons name (length params) *function-table*))
	  (emit ".global ~(~A~)" name)
	  (emit "~(~A~):" name)
	  (emit "push {fp, lr}")
	  (emit "mov fp, sp") ; allocate stack for local vars
	  (emit "push {r0, r1, r2, r3}")
	  (compile-form body params output)
	  (emit "mov sp, fp") ; dealocate stack space
	  (emit "pop {fp, lr}")
	  (emit "bx lr")))
       (lambda
	(let ((params (second obj))
	      (body   (third obj)))
	  ;; tree walk to identify captured variables
	  ;; if no captures, compile like ordinary function
	  ))
       (if
	(let ((elselab (gensym))
	      (endlab  (gensym)))
	  (assert (= 4 (length obj)))
	  (compile-form (second obj) envir output)
	  (emit "cmp r0, #0")
	  (emit "beq .L~A" elselab)
	  (compile-form (third obj) envir output)
	  (emit "b .L~A" endlab)
	  (emit ".L~A:" elselab)
	  (compile-form (fourth obj) envir output)
	  (emit ".L~A:" endlab)))
       (t (progn
	    (dolist (o (cdr obj))
	      (progn (compile-form o envir output)
		     (emit "push {r0}")))
	    (compile-fn obj output)))))
     (t (error "Can't compile ~A" obj))))
