(def *read-pos* 0)
(def *read-tail* 0)
(def *read-str* nil)

;(defun r-peek (off)
;  (if *read-str*
;    (->> (+ *read-pos* off)
;         (char-at *read-str*))))

(defun r-peek (off)
  (if *read-str*
    (char-at *read-str* (+ *read-pos* off))))

(defun r-next! ()
  (let (x (r-peek 0))
    (progn
      (def *read-pos* (inc *read-pos*))
      x)))

(defun r-get ()
  (substr *read-str* *read-tail* *read-pos*))

(defun wait-for (ch)
  (let (x (r-peek 0))
    (cond
      (nil? x) nil
      (equal? x ch) ch
      t (progn (r-next!) (wait-for ch)))))

(defun read-atom1 (state)
  (let (ch (r-peek 0))
    (cond
      (equal? state :string)
      (cond
        (nil? ch) (print "unexpected EOF in string")

        (and (equal? ch "\\")
             (or (equal? (r-peek 1) "\"")
                 (equal? (r-peek 1) "n")
                 (equal? (r-peek 1) "\\")))
        (progn (r-next!) (r-next!) (read-atom1 state))

        (equal? ch "\"")
        (progn (r-next!) (read-atom0 (r-get)))
        
        t (progn (r-next!) (read-atom1 state)))

      (or (nil? ch) (equal? ch ")") (whitespace? ch) (equal? ch "\n"))
      (read-atom0 (r-get))

      t (progn (r-next!) (read-atom1 state)))))

(defun read-atom ()
  (let (ch (r-peek 0))
    (cond
      (equal? ch "\"") (progn (r-next!) (read-atom1 :string))
      t (read-atom1 nil))))

(defun read-form ()
  (let (ch (r-peek 0))
  (cond
    (nil? ch) nil
    (whitespace? ch) (progn (r-next!) (read-form))
    (equal? ch ";") (progn (wait-for "\n") (read-form))

    (equal? ch "'") (progn (r-next!) (list (quote quote) (read-form)))

    (equal? ch "(") (progn (r-next!) (read-list nil))
    (equal? ch ")") (print "unexpected )")
    t (progn (def *read-tail* *read-pos*) (read-atom)))))

(defun read-list (acc)
  (let (ch (r-peek 0))
    (cond
      (nil? ch) (print "unexpected eof")
      (equal? ch ")") (progn (r-next!) (reverse acc))
      t (read-list (cons (read-form) acc)))))

(defun read (str)
  (progn
    (if str
      (progn
        (def *read-str* str)
        (def *read-pos* 0)))
    (read-form)))

(defun read-all0 (acc)
  (let (sexpr (read nil))
    (if sexpr
      (read-all0 (cons sexpr acc))
      (reverse acc))))

(defun read-all (path)
  (read-all0 (list (read (slurp path)))))

(defun nil? (p)
  (if p nil t))

(defun reverse0 (in acc)
  (if in
    (reverse0 (rest in) (cons0 (first in) acc))
    acc))

(defun reverse (in)
  (reverse0 in nil))
