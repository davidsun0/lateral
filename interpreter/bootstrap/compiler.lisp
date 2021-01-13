;;; reduces a tree to a list of lists
(defun semi-flatten0 (in acc)
  (if in
    (if (and (list? (first in))
             (list? (first (first in))))
      (semi-flatten0 (rest in) (semi-flatten0 (first in) acc))
      (semi-flatten0 (rest in) (cons (first in) acc)))
    acc))

(defun semi-flatten (in)
  (reverse (semi-flatten0 in nil)))

(def *gensym-count* 0)
(defun gensym (prefix)
;  (keyword (string prefix (def *gensym-count* (inc *gensym-count*)))))
   (keyword (str-cat prefix (string (def *gensym-count* (inc *gensym-count*))))))

(defun progn-deflate (name expr acc)
  (if expr
    (progn-deflate name
                   (rest expr)
                   (cons (list (if (rest expr)
                                 (list :pop)
                                 (cons nil nil))
                               (ir0 (if (rest expr) nil name) (first expr) nil))
                         acc))
    acc))

(defun or-deflate (name end-lab expr acc)
  (if expr
    (progn (print (ir0 nil (first expr) nil))
    (or-deflate name
                end-lab
                (rest expr)
                (cons
                (list (list :pop)
                      (list :jump-not-nil end-lab)
                      (list :dup)
                      (ir0 nil (first expr) nil))
                acc)))
    (cons (list (if name
                  (list :return)
                  (cons nil nil))
                (list :label end-lab)
                (list :push :nil))
          acc)))

(defun and-deflate (name false-lab expr acc)
  (if expr
    (and-deflate name
                 false-lab
                 (rest expr)
                 (cons (list (if (rest expr)
                               (list :jump-if-nil false-lab)
                               (cons nil nil))
                             (ir0 nil (first expr) nil))
                       acc))
    (let (end-lab (gensym "and-e"))
      (cons (list (if name
                    (list :return)
                    (cons nil nil))
                  (list :label end-lab)
                  (list :push :nil)
                  (list :label false-lab)
                  (list :goto end-lab))
            acc))))

(defun cond-deflate0 (name expr test-lab end-lab acc)
  (if expr
    (let (test     (first expr)
          branch   (second expr)
          next-lab (gensym "cond-"))
      (cond-deflate0 name (rest (rest expr)) next-lab end-lab
                     (cons (list (if name
                                   (cons nil nil)
                                   (list :goto end-lab))
                                 (ir0 name branch nil)
                                 (list :jump-if-nil next-lab)
                                 (ir0 nil test nil)
                                 (if test-lab
                                   (list :label test-lab)
                                   (cons nil nil)))
                           acc)))
    (cons (list (if name
                  (list :return)
                  (cons nil nil))
                (list :label end-lab)
                (list :push :nil)
                (list :label test-lab))
          acc)))

(defun let-deflate0 (bind-list acc)
  (if bind-list
    (let-deflate0 (rest (rest bind-list))
                 (cons (list (list :let-set (first bind-list))
                             (ir0 nil (second bind-list) nil))
                       acc))
    acc))

(defun let-deflate (name expr)
  (list
    (list :let-pop)
    (ir0 name (second expr) nil)
    (list :let-body)
    (let-deflate0 (first expr) nil)
    (list :let-bind)))

;; iterates along list, resolving nested lists with ir0
(defun ir1 (ast acc)
  (if ast
    (if (list? (first ast))
      (ir1 (rest ast)
           (concat (ir0 nil (first ast) nil) acc))
      (ir1 (rest ast)
           (cons (list :push (first ast)) acc)))
    acc))

(defun ir (name ast)
  (reverse (semi-flatten (ir0 name ast nil))))

;; first step in code processing
;; turns a tree of lisp code into a stack-based intermediate representation
;; name doubles as a flag for if the expression is in the tail position
(defun ir0 (name ast acc)
  (cond
    (nil? ast) (reverse acc)
    (and (not (list? ast)) name) (list (list :return) (list :push ast))
    (not (list? ast)) (list (list :push ast))

    (equal? (first ast) (quote if))
    (let (false-label (gensym "if-f")
          end-label (if name (gensym "if-e")))
      (list
        (if (not name) (list :label end-label) (cons nil nil))
        (if (= (length ast) 4)
          ;; has an else branch
          (list (ir0 name (nth 3 ast) nil)
                (if (not name)
                  (list :goto end-label)
                  (cons nil nil)))
          ;; no else branch
          (list (if name
                  (list :return)
                  (cons nil nil))
                (list :push :nil)))
        (list :label false-label)
        (ir0 name (nth 2 ast) nil)
        (list :jump-if-nil false-label)
        (ir0 nil (nth 1 ast) nil)))

    (equal? (first ast) (quote and))
    (and-deflate name (gensym "and-f") (rest ast) nil)

    (equal? (first ast) (quote or))
    (or-deflate name (gensym "or-e") (rest ast) nil)

    (equal? (first ast) (quote cond))
    (cond-deflate0 name (rest ast) nil (gensym "cond-e") nil)

    (equal? (first ast) (quote progn))
    (progn-deflate name (rest ast) nil)

    (equal? (first ast) (quote let))
    (let-deflate name (rest ast))

    (equal? (first ast) (quote quote))
    (list (list :push :symbol (second ast)))

    (equal? (first ast) name)
    (cons (list :tail-recur :argc (dec (length ast)))
          (ir1 (rest ast) nil))

    name (cons (list :return) (ir0 nil ast acc))

    t
    (cons (list :funcall (first ast) :argc (dec (length ast)))
          (ir1 (rest ast) nil))))

;; arglist is local variables of the environment just outside the let expression
;; letlist is the local variables of the let expression
(defun resolve-syms0 (ir-list arglist letlist acc)
  (if ir-list
    (let (expr (first ir-list))
      (cond
        ;; resolve symbols in inner let symbol
        (equal? (first expr) :let-bind)
        (let (ir-and-acc (resolve-syms0 (rest ir-list)
                                        (concat arglist letlist)
                                        nil
                                        (cons (list :let-push) acc)))
          (resolve-syms0 (first ir-and-acc) arglist nil (second ir-and-acc)))

        ;; storing a local variable in a let binding
        (equal? (first expr) :let-set)
        ;; update letlist and get local var's index
        (let (inlet? (index (nth 1 expr) letlist)
              letlist (if inlet? letlist (append letlist (nth 1 expr)))
              argidx (+ (length arglist)
                        (if inlet? inlet? (dec (length letlist)))))
            (resolve-syms0
              (rest ir-list)
              arglist
              letlist
              ;; store marker for number of local variables at current position
              (cons (list :local-count
                          (gensym "letc")
                          (+ (length letlist) (length arglist)))
              (cons (list :store :arg-num argidx) acc))))

        ;; ignore let-body
        (equal? (first expr) :let-body)
        (resolve-syms0 (rest ir-list) arglist letlist acc)

        ;; return (unprocessed-syms, processed-syms)
        (equal? (first expr) :let-pop)
        (list (rest ir-list) (cons (list :let-pop
                                         (gensym "letp")
                                         (length arglist))
                                   acc))

        t
        (resolve-syms0
          (rest ir-list)
          arglist
          letlist
          (cons
            (cond
              (not (equal? :push (first expr))) expr
              (equal? (nth 1 expr) (quote nil)) (list :push :nil)
              (equal? (nth 1 expr) (quote t)) (list :push :true)
              (int? (nth 1 expr)) (list :push :int-const (nth 1 expr))
              (char? (nth 1 expr)) (list :push :char-const (nth 1 expr))
              (string? (nth 1 expr)) (list :push :str-const (nth 1 expr))

              (symbol? (nth 1 expr))
              (let (letidx (index (nth 1 expr) letlist)
                    idx (if letidx
                          (+ (length arglist) letidx)
                          (index (nth 1 expr) arglist)))
              (if idx
                (list :push :arg-num idx)
                (list :push :envir-sym (nth 1 expr))))

              t expr)
            acc))))
    (list :asdfasdf acc)))

(defun resolve-syms (ir-list arglist)
  (let (a (resolve-syms0 ir-list arglist nil nil))
    (reverse (second a))))
