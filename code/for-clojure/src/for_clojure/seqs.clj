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