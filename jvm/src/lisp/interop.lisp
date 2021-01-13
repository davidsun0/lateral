(defclass "MyCanvas" ; class name should be Symbol?
  (metastuff (:extends "javax.swing.JPanel"))

  (defmethod paintComponent
    "(Ljava/awt/Graphics;)V"
    (meta... :protected)
    (bytecodes)))

(defclass MyCanvas (:public :extends "javax/swing/JPanel")
  (defmethod paintComponent
    "(Ljava/awt/Graphics;)V"
    (:protected)
    (my-function (asm-quote :aload1))))

(defclass ArraySequence
  (:extends Sequence)

  (deffield values "[Ljava/lang/Object;" (:private))
  (deffield index  "I"                   (:private))
  (deffield next   Sequence              (:private))

  (defmethod first
    "()Ljava/lang/Object;"
    (:public)
    (asm-tree (aaload (getvirtual aload0 values)
                      (getvirtual aload0 index)))))
