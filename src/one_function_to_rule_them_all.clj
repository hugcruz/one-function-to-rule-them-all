(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [acc val] (str acc " " val)) a-seq)
))

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq)      []
    :else               (reduce
                          (fn [acc val] (conj (conj acc x) val))
                          [(first a-seq)]
                          (rest a-seq))
))

(defn my-count [a-seq]
  (reduce (fn [acc _] (inc acc)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc val] (cons val acc)) '() a-seq))

(defn min-max-element [a-seq]
  [
    (reduce min a-seq)
    (reduce max a-seq)
  ])

(defn insert [sorted-seq n]
  (concat
    (take-while #(< % n) sorted-seq)
    (list n)
    (drop-while #(< % n) sorted-seq))
)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)
))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x]   (* x -1))
  ([x y] (- x  y))
)

(defn count-params [& x]
  (count x))

(defn my-*
  ([]           1)
  ([x]          x)
  ([x y]        (* x y))
  ([x y & more] (reduce * (* x y) more))
)

(defn pred-and
  ([]           (fn [x] true))
  ([p]          p)
  ([p q]        (fn [x] (and (p x) (q x))))
  ([p q & more] (reduce
                  (fn [acc pred] (fn [x] (and (acc x) (pred x))))
                  (fn [x] (and (p x) (q x)))
                  more))
)

(defn my-map [f & a-seq]
  (reduce f a-seq))
