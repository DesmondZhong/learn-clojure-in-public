(ns for-clojure.seqs)

;;;;;;;;;;;;;;;;;;;;;; Elementary ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 64 Intro to Reduce

;; 156 Map Defaults
(
 (fn [x ks]
   (into {} (map #(vector % x) ks)))
 0 [:a :b :c])

;; 32 Duplicate a Sequence
(def xs [[1 2] [3 4]])


;; use interleave
(
 (fn [xs] (interleave xs xs))
 xs)

;; use reduce
(reduce #(conj %1 %2 %2) [] xs)

;; use mapcat
(mapcat #(list % %) xs)
;; the above is equivalent to
(apply concat (map (fn [x] [x x]) xs))

;; 30 Compress a Sequence
(def xs [1 1 2 3 3 2 2 3])

(
   #(map first (partition-by identity %))
   xs)
;; this is added in Clojure 1.7
(dedupe xs)

;; 33 Replicate a Sequence
(def xs [[1 2] [3 4]])
;; use reduce
(
   #(reduce (fn [coll x] (apply conj coll (repeat %2 x))) [] %1)
   xs 4)
;; use mapcat
(
   #(mapcat (fn [x] (repeat %2 x)) %1)
   xs 4)

;; 45 Intro to Iterate

;; 31 Pack a Sequence

;; 41 Drop Every Nth Item
;; use partition, understand input arguments
(def xs [1 2 3 4 5 6 7 8])
(
   (fn [xs n] (flatten (map drop-last (partition n n [0] xs))))
 xs 3
   )

(partition 3 3 [0] xs)

;; 157 Indexing Sequences
(fn p157
   ([coll]
    (p157 [] 0 coll))
   ([result idx coll]
    (if (empty? coll)
       result
       (recur (conj result [(first coll) idx]) (inc idx) (rest coll)))))

(p157 [:a :b :c])

;; map-indexed takes a function which should take two arguments index and item
(map-indexed #(vector %2 %1) [:a :b :c])

;; 147 Pascal's Trapezoid
(second
 (
    #(iterate (fn [coll]
                 (conj
                    (vec (cons (first coll)
                               (map +' (rest coll) (drop-last coll))))
                    (last coll))) %)
    [2 3 2]))

(defn pascal [coll]
   (conj
      (vec (cons (first coll)
                 (map + (rest coll) (drop-last coll))))
      (last coll)))

(pascal [2 3 2])

(second
   (
      #(iterate (fn [coll]
                   (map +' (concat [0] coll)
                          (concat coll [0]))) %)
      [2 3 2]))


;; 44 Rotate Sequence
;; Note that `mod` returns the remainder,
;; it is slightly different from `rem`
(
 (fn [x coll]
    (let [n (mod x (count coll))]
       (concat (drop n coll) (take n coll))))
 2 [1 2 3 4 5])

;; we can also achieve it with `cycle`

;; 43 Reverse Interleave
;; note thtat take-nth can never take every 0th!
(
 (fn [coll n]
    (for [i (range n)] (take-nth n (drop i coll))))
 [1 2 3 4 5 6] 2)

(take-nth 0 (drop 0 [1 2 3 4 5 6]))

;; 50 Split by Type
(vals (group-by type [1 :a 2 :b 3 :c]))

((comp vals (partial group-by type)) [1 :a :b 2 3 :c])

;; 110 Sequence of pronunciations
(take 3
      (rest
       (iterate
         (fn [x]
           (apply concat (map #(vector (count %) (first %)) (partition-by identity x))))
         [1]))
      )

;; 93 partially flatten a sequence
;; tree seq internally do a recursion
(defn pflatten [tree]
  (if (every? sequential? tree)
    (mapcat pflatten tree)
    [tree]))
(pflatten '((1 2)((3 4)((((5 6)))))))

;; 28 flatten a sequence
(tree-seq sequential? seq ["a" ["b"] "c"])
(filter (complement sequential?) (tree-seq sequential? seq ["a" ["b"] "c"]))
(tree-seq seq? identity '((1 2 (3)) (4)))

;; 112 Sequs Horribilis
