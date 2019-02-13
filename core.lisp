(def defmacro
  (macro (name args expr)
    `(def ~name (macro ~args ~expr))))

(defmacro defn (a b c)
  `(def ~a (fn ~b ~c)))

(defn inc (n)
  (+ n 1))
