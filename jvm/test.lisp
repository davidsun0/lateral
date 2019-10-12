(defun method-type (argc)
  (apply string (cons "(" (repeat0 "Ljava/lang/Object;" argc
                                   (list ")Ljava/lang/Object;")))))
