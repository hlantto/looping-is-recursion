(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [acc n] (if (< n 2)
                               acc
                               (recur (* acc base) (dec n))))]
      (helper base exp))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [b-seq] (if (= 1 (count b-seq))
                               (first b-seq)
                               (recur (rest b-seq))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (if (not (= (count seq1) (count seq2)))
    false
    (let [helper (fn [a-seq b-seq] (if (empty? a-seq)
                                     true
                                     (if (not (= (first a-seq) (first b-seq)))
                                       false
                                       (recur (rest a-seq) (rest b-seq)))))]
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (let [cnt (count a-seq)]
    (loop [n 0
           r-seq a-seq]
      (if (= n cnt)
        nil
        (if (pred (first r-seq))
          n
          (recur (inc n) (rest r-seq)))))))

(defn avg [a-seq]
  (loop [acc 0
         r-seq a-seq]
    (if (empty? r-seq)
      (/ acc (count a-seq))
      (recur (+ acc (first r-seq)) (rest r-seq)))))

(defn parity [a-seq]
  (loop [a-set #{}
         r-seq a-seq]
    (if (empty? r-seq)
      a-set
      (if (contains? a-set (first r-seq))
        (recur (disj a-set (first r-seq)) (rest r-seq))
        (recur (conj a-set (first r-seq)) (rest r-seq))))))

(defn fast-fibo [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (loop [n1 1
                 n2 0
                 n-down (dec n)]
            (if (zero? n-down)
              n1
              (recur (+ n1 n2) n1 (dec n-down))))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         r-seq a-seq
         n 0]
    (if (or (empty? r-seq) (contains? a-set (first r-seq)))
      (take n a-seq)
      (recur (conj a-set (first r-seq)) (rest r-seq) (inc n)))))

