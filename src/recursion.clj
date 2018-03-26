(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
    )
  )

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false
    )
  )

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))
    )
  )

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element(rest a-seq)))
    )
  )

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2)
      seq-1
      seq-2)
    )
  )

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence(rest a-seq)))
    )
  )

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
      )
    )
  )

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    )

  )

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
    )
  )

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
    )
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    )
  )

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    )
  )

(defn power [n k]
  (cond
    (zero? n) 0
    (zero? k) 1
    :else (* n (power n (dec k)))
    )
  )

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
    )
  )

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) '()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    )
  )

(defn my-range [up-to]
  (cond
    (= up-to 0) '()
    :else (cons (dec up-to) (my-range (dec up-to)))
    )
  )

(defn tails [a-seq]
  (cond
    (empty? a-seq) (cons a-seq '())
    :else (cons (sequence a-seq) (tails (rest a-seq)))
    )
  )

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))
    )
  )

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))
    )
  )

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-el (first a-seq)
           new-freqs (if (contains? freqs current-el)
                      (assoc freqs current-el (inc (get freqs current-el)))
                      (assoc freqs current-el 1)
                      )]
      (my-frequencies-helper new-freqs (rest a-seq))
      )
    )
  )

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ""
    (let [elem (key (first a-map))
          freq (val (first a-map))]
      (concat (repeat freq elem) (un-frequencies (rest a-map)))
      )
    )
  )

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (zero? n) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))
    )
  )

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))
    )
  )

(defn halve [a-seq]
  (cond
    (empty? a-seq) '()
    :else (let [n1 (int (/ (count a-seq) 2))
                n2 (- (count a-seq) n1)]
            (vector (my-take n1 a-seq) (reverse (my-take n2 (reverse a-seq))))
            )
    )
  )

(defn better-halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)
     (my-drop half a-seq)]))


(defn seq-merge [a-seq b-seq]
  (let [firsta (first a-seq)
        firstb (first b-seq)]
    (cond
      (empty? (and a-seq b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< firsta firstb) (cons firsta (seq-merge (rest a-seq) b-seq))
      :else (cons firstb (seq-merge (rest b-seq) a-seq))
      )
    )
  )

(defn merge-sort [a-seq]
  (let [half (halve a-seq)
        start (first half)
        end (first (reverse half))]
    (cond
      (or (empty? a-seq) (singleton? a-seq)) a-seq
;;       :else (apply seq-merge (map merge-sort half))
      :else (seq-merge (merge-sort start) (merge-sort end))
      )
    )
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

