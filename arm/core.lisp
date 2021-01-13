(defun not (p)
  (if p nil t))

(defun reverse0 (in acc)
  (if in
    (reverse0 (cdr in) (cons (car in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))

(reverse '(5 4 3 2 1 0))

(defun map0 (f in acc)
  (if in
    (map0 f (cdr in) (cons (f (car in)) acc))
    acc))

(defun map (f in)
  (reverse (map0 f in nil)))

(defun map (f in)
  (labels ((map0 (f in acc)
             (if in
               (map0 f (cdr in) (cons (f (car in)) acc))
               acc)))
    (map0 f in nil)))

(let ((a 'b) (b 'c))
  (cons b (cons a nil)))
