(ns looping-is-recursion)

(defn power [base exp]
  (let [acc 1
        n exp
        helper (fn [acc n]
                 (cond (zero? n) 1
                       (= 1 n) acc
                       (neg? n) (/ (power base (- n)))
                       :else (recur (* acc base) (dec n))))]
    (helper base exp)))

;E2
(defn last-element [a-seq]
  (let [helper (fn [a]
                 (cond (empty? (rest a)) (first a)
                       :else   (recur (rest a))))
        ]
    (helper a-seq)))

;E3
(defn seq= [seq1 seq2]
  (let[helper (fn [a b]
                (let [x (first a)
                      y (first b)
                      xr (rest a)
                      yr (rest b)]
                  (cond (and (empty? a) (empty? b)) true
                        (or (empty? a) (empty? b)) false
                        (not (= a b)) false
                        :else (recur xr yr))
                  ))]
    (helper seq1 seq2))
  )

;E4
(defn find-first-index [pred a-seq]
  (loop [n 0
         r a-seq]
    (cond (empty? r) nil
          (pred (first r)) n
          :else (recur (inc n) (rest r)))
    ))

;E5
(defn avg [a-seq]
  (loop [n 0
         s 0
         r a-seq]
    (cond (empty? r) (if (zero? n)
                       0
                       (/ s n)
                       )
          :else (recur (inc n) (+ s (first r)) (rest r)))
    ))

;E6
(defn toggle [a-set elem]
  "Parityn apufunktiona"
  (if (contains? a-set elem )
    (disj a-set elem )
    (conj a-set elem )
    )
  )
(defn parity [a-seq]
  (loop [acc #{}
         r a-seq]
    (cond (empty? r) acc
          :else (recur (toggle acc (first r)) (rest r)) )))
;E7
(defn fast-fibo [n]
  (loop [i 0
         a -1
         b 1]
    (let [c (+ a b)]
      (cond (>= i n) c
            :else (recur (inc i) b c)))))

;E8
; (defn set-to-list [a]   (conj (rest a) (first a)))
(defn cut-at-repetition [a-seq]
  (loop [coll #{}
         acc []
         r a-seq]
    (cond (empty? r) acc
          (contains? coll (first r)) acc
          :else (recur (conj coll (first r)) (conj acc (first r)) (rest r)) )))


