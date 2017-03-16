(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n]
                   (if (zero? n)
                       acc
                       (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn last-element [a-seq]
  (let [helper (fn [seq]
                   (if (or (singleton? seq)
                           (empty? seq))
                       (first seq)
                       (recur (rest seq))))]
    (helper a-seq)))

(defn seq= [a-seq b-seq]
  (loop [seq1 a-seq
         seq2 b-seq]
    (if (and (empty? seq1)
             (empty? seq2))
        true
        (if (or (empty? seq1)
                (empty? seq2))
            false
            (and (= (first seq1) (first seq2))
                 (recur (rest seq1) (rest seq2)))))))

(defn find-first-index [predicate a-seq]
  (loop [s a-seq
         n 0]
    (if (empty? s)
        nil
        (if (predicate (first s))
            n
            (recur (rest s) (inc n))))))

(defn avg [a-seq]
  (loop [sum 0
         num 0
         seq a-seq]
    (if (empty? seq)
        (if (= num 0)
            0
            (/ sum num))
        (recur (+ sum (first seq)) (inc num) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         b-seq a-seq]
    (if (empty? b-seq)
        a-set
        (recur (toggle a-set (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (loop [acc 0
         nc n
         n0 1
         n1 0
         n2 0]
    (if (zero? nc)
        acc
        (recur (+ n0 n1 n2) (dec nc) 0 (+ n0 n1 n2) n1))))

(defn cut-at-repetition [v]
  (loop [res []
         left v]
    (if (empty? left)
        res
        (if (contains? (into #{} res) (first left))
            res
            (recur (conj res (first left)) (rest left))))))

