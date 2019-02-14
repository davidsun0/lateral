(def defmacro
  (macro (name args expr)
    `(def ~name (macro ~args ~expr))))

(defmacro defn (a b c)
  `(def ~a (fn ~b ~c)))

(defn inc (n)
  (+ n 1))

(defn mult (a b)
  (loop (acc 0 n 0)
    (if (= n b)
      acc
      (recur (+ acc a) (+ n 1)))))
