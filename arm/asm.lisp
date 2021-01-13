(defpackage :lateral
  (:use :cl))
(in-package :lateral)

;; (declaim (optimize (debug 3)))

(defparameter *register-count* 10)
(defparameter *built-ins* '(add))

(defmacro consf (new-head list)
  "Modifies a reference by consing a new value to it"
  `(setf ,list (cons ,new-head ,list)))

(defun compile-top (form)
  (destructuring-bind (ops var-count) (compile-form form)
    (format t "~{~A~}" (mapcar #'assemble-op (peephole (register-allocate ops var-count))))))

(defun compile-unit (form)
  ;; top level can accept defs and defuns
  ;; collect all other statements into main / start
  (format t "~{~A~}"
  (cond
    ((not (consp form)) (error "can't compile"))
    ((eq (first form) 'defun)
     ;; (defun name (args ...) body)
     (if (> (length (third form)) 4)
	 (error "can't compile functions with more than 4 params"))
     (let ((arg-envir (loop for arg in (third form)
			    for i from 0
			    collect (cons arg i)))
	   (arg-binds (loop for arg in (third form)
			    for i from 0
			    collect `(mov (v ,i) (r ,i)))))
       (concatenate 'list
		    (list (format nil "~(~A~):~%push {fp, lr}~%" (second form)))
		    ;; move arguments from registers into variables
		    ;; create binding of argument variables
		    (destructuring-bind (ops vars)
			(compile-form (fourth form)
				      :envir arg-envir
				      :var-num (length (third form)))
		      (let ((ops (concatenate 'list arg-binds ops)))
			(mapcar #'assemble-op
				(peephole (register-allocate ops vars)))))
		    (list (format nil "pop {fp, lr}~%bx lr~%~%")))))
    (t (error "can't compile")))))

(defun compile-form (form &key (envir nil) (var-num 0))
  (let ((var-num (1- var-num))
	(lab -1)
	(instr-list '()))
    (labels
	((emit (f)
	   (consf f instr-list))
	 (var (x) `(v ,x))
	 (walk (f envir &optional (result-var nil))
	   (let ((result (if result-var result-var (incf var-num))))
	     (cond
	       ((numberp f)
		(emit `(mov (v ,result) (const ,f)))
		result)

	       ((symbolp f)
		(decf var-num) ; symbol lookup does not need new variable
		;; return variable id of lookup
		(let ((lookup (cdr (assoc f envir))))
		  (unless lookup
		    (error "var ~A not found in envir ~A" f envir))
		  lookup))
	       
	       ((not (consp f))
		(error "Can't compile ~A of type ~A" f (type-of f)))

	       ((eq 'if (first f))
		(let ((test     (walk (second f) envir))
		      (else-lab (incf lab))
		      (end-lab  (incf lab)))
		  (emit `(jz (lab ,else-lab) (v ,test)))
		  (walk (third f) envir result) ; then branch
		  (emit `(jmp (lab ,end-lab)))
		  (emit `(lab ,else-lab))
		  (walk (fourth f) envir result) ; else branch
		  (emit `(lab ,end-lab))
		  result))

	       ((and (consp (first f)) (eq 'lambda (caar f)))
		;; ((lambda ...) ...)
		;; lambda call - create environment with binding and compile body
		(decf var-num) ; lambda evaluation does not need new variable
		(let ((l-params (second (first f)))
		      (l-args   (rest f))
		      (l-body   (third (first f)))
		      (l-envir  envir))
		  (assert (and (consp l-params) (= (length l-params) (length l-args))))
		  ;; simulateously compile argument terms and create environment
		  (loop for param in l-params
			for arg   in l-args
			;; build alist with var bindings
			do (consf (cons param (walk arg envir)) l-envir))
		  (walk l-body l-envir)))

	       ((member (first f) *built-ins*)
		(emit (concatenate 'list
				   (list (first f) (var result))
				   (loop for term in (cdr f)
					 collect (var (walk term envir)))))
		result)

	       (t
		(assert (<= (length (rest f)) 4))
		;; compile arguments and move to arg registers
		(loop for resv in (mapcar (lambda (term) (walk term envir)) (rest f))
		      for i from 0
		      do (emit `(mov (r ,i) (v ,resv))))
		;; subroutine call
		(emit (concatenate 'list
				   (list (list 'funcall (first f)) '(r 0))
				   (loop for i from 0 upto 3
					 collect `(r , i))))
		(emit `(mov (v ,result) (r 0)))
		result)))))
      ;; move result into return register
      (emit `(mov (r 0) (v ,(walk form envir))))
      (setf instr-list (reverse instr-list))
      #|
      (loop for instr in instr-list
	    for i from 0 by 2 do
	  (format t "[~A] ~A~%" i instr))
      |#
      (list instr-list (1+ var-num)))))

(defun variablep (term)
  "Tests if an op term represents a variable."
  (and (consp term) (eq (first term) 'v)))

(defun registerp (term)
  "Tests if an op term represents a register."
  (and (consp term) (eq (first term) 'r)))

(defmacro update-interval (itvls slot op-num)
  "Updates the lifetime interval list with a new end value."
  (let ((slotx (gensym)))
  `(let ((,slotx (second ,slot)))
     (if (first (aref ,itvls ,slotx))
	 ;; update end of existing interval
	 (setf (first (aref ,itvls ,slotx)) ,op-num)
	 ;; there was a hole, start a new section
	 (setf (aref ,itvls ,slotx)
	       (cons ,op-num (cons ,op-num (aref ,itvls ,slotx))))))))

(defmacro interval-hole (itvls slot)
  "Creates a hole in the lifetime interval."
  `(if (first (aref ,itvls (second ,slot)))
       (consf nil (aref ,itvls (second ,slot)))))

(defun trim-interval (itvl)
  "Reverses a list and removes leading and trailing nils."
  (labels ((trim (i)
	     (if (first i)
		 i
		 (trim (rest i)))))
    (trim (reverse (trim itvl)))))
	      
(defun var-interval (op-list var-count)
  "Calculates variable lifetime intervals."
  (let ((var-list (make-array var-count :initial-element nil)))
    (loop for op in op-list
	  for i from 0 by 2
	  ;; variable is used as argument -> extend the life of var
	  do (loop for term in (nthcdr 2 op)
		   when (variablep term)
		     do (update-interval var-list term i))
	  ;; variable is written to -> var was previously dead
	  when (variablep (second op))
	    do (progn
		 (interval-hole var-list (second op))
		 (update-interval var-list (second op) (1+ i))))
    (loop for itvl across var-list
	  for i from 0
	  collect (cons (list 'v i) (trim-interval itvl)))))

(defun reg-interval (op-list)
  "Calculates register lifetime intervals."
  (let ((reg-list (make-array *register-count* :initial-element nil))
	;; (hints '()) todo: hint placement of registers
	)
    (loop for op in op-list
	  for i from 0 by 2
	  ;; register used as argument -> register is dead
	  do (loop for term in (nthcdr 2 op)
		   when (registerp term)
		     do (interval-hole reg-list term))
	  ;; result is placed in register -> register is live
	  when (registerp (second op))
	    do (update-interval reg-list (second op) (1+ i)))
    ;; formatting the register lifetimes to be like the variables'
    (loop for reg across reg-list
	  for ri from 0
	  when reg
	    do (setf (aref reg-list ri) (cons 'r (trim-interval reg))))
    reg-list))

(defun interval-intersectsp (a b)
  "Tests if two lifetime intervals intersect."
  (labels ((helper (x y)
	     (cond
	       ((not (and x y)) nil)
	       ((< (second x) (first y)) (helper (nthcdr 3 x) y))
	       ((< (second y) (first x)) (helper x (nthcdr 3 y)))
	       (t t))))
    (helper (rest a) (rest b))))

(defun register-allocate (op-list var-count)
  "Assigns a physical register to each variable in op-list."
  (let ((lifetimes (sort (var-interval op-list var-count)
			 #'< :key #'second))
	(rlives (reg-interval op-list))
	(active '())
	(assigns '()))
    (loop for curr in lifetimes do
      (let ((blocked (make-array *register-count* :initial-element nil))
	    (pos (second curr))
	    (new-active '()))
	(loop for itvl in active
	      ;; update active intervals (necessary?)
	      do (cond
		   ((> (third itvl) pos)
		    (consf itvl new-active))
		   ((nthcdr 3 itvl)
		    (consf (cons (first itvl) (nthcdr 3 itvl)) new-active)))
	      ;; check for intersections
	      when (interval-intersectsp curr itvl)
		do (setf (aref blocked (first itvl)) t))
	(setf active new-active)
	(let ((res (loop for reg across blocked
			 for ri from 0
			 unless (or reg (interval-intersectsp (aref rlives ri) curr))
			   return ri)))
	  (if res
	      (progn
		(if (> res 3) (error "callee saved registers unimplemented"))
		(consf (cons (cadar curr) res) assigns)
		(consf (cons res (rest curr))  active))
	      (error "register spill unimplemented")))))
    (loop for op in op-list
	  collect (loop for term in op
			collect (if (variablep term)
				    (list 'r (cdr (assoc (second term) assigns)))
				    term)))))

(defun peephole (op-list)
  "Applies peephole optimizations on assembly code."
  ;; form fused operations e.g. intermediate bitshift
  ;; could probably make better peephole optimizations with liveliness information
  ;; maybe maplist is better or simply recursion
  (loop for ops on op-list
	append
	(let ((curr (first ops)))
	  (cond
	    ;; delete mov operations if source = dest
	    ((and (eq 'mov (first curr)) (equal (second curr) (third curr)))
	     nil)
	    (t
	     (list curr))))))

(defun assemble-op (op)
  "Assembles ARM32 instructions from in-memory representation."
  (labels ((emit-term (term)
	     (format nil
		     (case (first term)
		       ((r) "r~A")
		       ((const) "=~A")
		       ((lab) "label~A")
		       (t (error "Can't assemble term ~A" term)))
		     (second term))))
    (if (and (consp (first op)) (eq 'funcall (caar op)))
	(format nil "bl ~(~A~)~%" (cadar op))
	(case (first op)
	  ((mov)
	   (format nil
		   (if (and (consp (third op)) (eq 'const (first (third op))))
		       "ldr ~{~A~^, ~}~%"
		       "mov ~{~A~^, ~}~%")
		   (mapcar #'emit-term (cdr op))))
	  ((lab) (format nil "~A: " (emit-term op)))
	  ((jmp) (format nil "b ~A~%" (emit-term (second op))))
	  ((jz)  (format nil "cmp ~A, #0~%beq ~A~%"
			 (emit-term (third op))
			 (emit-term (second op))))
	  ((add) (format nil "add ~{~A~^, ~}~%" (mapcar #'emit-term (cdr op))))
	  (t (error "Can't assemble ~A" op))))))
