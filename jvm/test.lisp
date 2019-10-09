(defun main ()
  (print "Hello World!"))

(defun iden (x)
  (if x
    nil
    t))

(defun not (x)
  (cond
    x nil
    t x))

(defun test (x)
  (if "Hello World!"
    (print "Yes")
    (print "No")))

(defun test (x)
  (cond 
    (huh? x) nil
    x t
    t nil))
