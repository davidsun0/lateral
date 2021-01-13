(defun third (x)
  (first (rest (rest x))))

(defun fourth (x)
  (first (rest (rest (rest x)))))

(defun compile (form)
  (cond
    (list? form)
    (case (first form)
      'if (let (elselab (gensym "else")
                endlab  (gensym "end"))
            `(,(compile (second form))
              :ifnull
              ,elselab
              ,(compile (third form))
              (:goto ,endlab)
              (:label ,elselab)
              ,(compile (fourth form))
              (:label ,endlab)))
      (cons (list 'funcall (first form))
            (map compile (rest form))))

    t form))
