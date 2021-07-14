(ns for-clojure.seqs-core-functions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Easy ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 34 implement range without using range
;; it is easy to do it with recursion
(defn my-range
  ([start end]
   (my-range [] start end))
  ([coll start end]
   (if (= start end)
     coll
     (recur (conj coll start) (inc start) end))))

(my-range -2 2)

;; but there a much simpler way
;; the idea is to generate a lazy-seq and take the right number of it!

(defn my-range2
  [start end]
  (take (- end start) (iterate inc start)))

(my-range2 -2 2)

;; 39 interleave two seqs without using interleave
;; this is my first attempt, it seems that the recursion version cannot
;; solve the issue that that the first coll and second coll
;; are not treated in equally
(defn my-interleave
  ([coll1 coll2]
   (my-interleave [] coll1 coll2))
  ([result coll1 coll2]
   (if (empty? coll2)
     result
     (recur (conj result (first coll1)) coll2 (rest coll1)))))

(my-interleave [1 2] [3 4 5 6])
;; should return [1 3 2 4]

(my-interleave [1 2 3 4] [5])
;; should return [1 5]

;; actually to do it recursively, we don't need to swap the place of coll1 and coll2
;; if we add two elements in one call, the above problem does not exist
(defn my-interleave-2
  ([coll1 coll2]
   (my-interleave-2 [] coll1 coll2))
  ([result coll1 coll2]
   (if (or (empty? coll1) (empty? coll2))
     result
     (recur (conj result (first coll1) (first coll2))
            (rest coll1)
            (rest coll2)))))

(my-interleave-2 [1 2] [3 4 5 6])

(my-interleave-2 [1 2 3 4] [5])

;; the correct thing to do is map cat
;; this is a good example of using map to take more than one seq
(
  #(mapcat vector %1 %2)
  [1 2] [3 4 5 6])

;; 40 interpose a seq without using interpose
(defn my-interpose
  [x coll]
  (drop-last
   (interleave coll
               (take (count coll) (repeat x)))))

(my-interpose 0 [1 2 3])

;; I did succeed but I forget that interleave takes care of the truncation
;; automatically! we can use the lazy-seq directly!
(defn my-interpose-2
  [x coll]
  (drop-last (interleave coll (repeat x))))

(my-interpose-2 0 [1 2 3])

;; what is the difference between drop-last and butlast?
;; butlast cannot drop multiple elements from a coll
;; drop-last can drop multiple elements from a coll

;; 49 split a sequence without using split-at
(defn my-split-at
  [n coll]
  (vector (take n coll) (take-last (- (count coll) n) coll)))

(my-split-at 3 [1 2 3 4 5 6])

;; well... take and drop can achieve the same task
#(vector (take %1 %2)
         (drop %1 %2))

;; an elegant way to solve this problem!
;; use juxtaposition!
((juxt take drop) 3 [1 2 3 4 5 6])

;; 62 re-implement iterate
;; it seems lazy-seq is usually used along with cons???
(defn my-iterate
  [f x]
  (lazy-seq (cons x (my-iterate f (f x)))))

(take 5 (my-iterate #(* 2 %) 1))

;; 19 last element without using last
#(first (reverse %))

;; 21 nth element without using nth
#(first (drop %2 %1))

;; or you can turn it into a vector
#(get (vec %1) %2)

;; 22 count a sequence without using count

;; standard recursion in clojure
(defn my-count
  ([coll]
   (my-count 0 coll))
  ([n coll]
   (if (empty? coll)
     n
     (recur (inc n) (rest coll)))))

(my-count "Hello World")

;; a smart way!
(defn my-count-2
  [coll]
  (reduce (fn [sum _] (inc sum)) 0 coll))

(my-count-2 "Hello World")


;; 23 reverse a sequence without using reverse rseq
(defn my-reverse
  [coll]
  (reduce conj () coll))

(my-reverse [1 2 3 4 5])

;; you can also use into (with list) since items are conj into the target
(into '() [1 2 3 4 5])

;; we can also leverage conj directly
(apply conj () [1 2 3 4 5])


;;;;;;;;;;;;;;;;;;;; Medium ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 55 count occurrences without frequencies
;; directly construct the map incrementally
(defn my-frequencies
  [coll]
  (reduce (fn [m x]
            (if (get m x)
              (assoc m x (inc (get m x)))
              (assoc m x 1)))
          {} coll))

(my-frequencies [1 1 2 3 2 1 1])

;; the previous version can be simplified into the following
(defn my-frequencies-2 [coll]
  "Counts occurence of each character in s"
  (reduce
    (fn [m k]
      (update-in m [k] (fnil inc 0)))
    {}
    coll))
;; Note use of fnil above
;; - returns 0 if nil is passed to inc (avoids null pointer exception)
((fnil inc 0) nil)
;; => 1


;; actually we might be able to simplify the process further
(group-by identity [1 1 2 3 2 1 1])
;; => {1 [1 1 1 1], 2 [2 2], 3 [3]}
(partition-by identity [1 1 2 3 2 1 1])
;; => ((1 1) (2) (3) (2) (1 1))

(defn my-frequencies-3
  [coll]
  (->> coll
       (group-by identity)
       (map #(vector (first %) (count (second %))))                 ;; map sends a key value pair from a hash-map
       (into {})))

(my-frequencies-3 [1 1 2 3 2 1 1])

;; 56 Find Distinct Items wihtout using distinct
(defn my-distinct
  [coll]
  (->> coll
       (group-by identity)
       (map first)))

(my-distinct [1 2 1 3 1 2 4])

(my-distinct '([2 4] [1 2] [1 3] [1 3]))

(my-distinct (range 10))
;; => (0 7 1 4 6 3 2 9 5 8)

(set (range 10))
;; => #{0 7 1 4 6 3 2 9 5 8}

;; the above version fails since it cannot maintain the order in some cases
(defn my-distinct-2
  [coll]
  (reduce (fn [res x]
            (if ((set res) x)                               ;; we can use the set as a verb to check existence of elements
              res
              (conj res x)))
          [] coll))

(my-distinct-2 (range 10))

(my-distinct-2 [1 2 1 3 1 2 4])

;; essentially what we need is a vector as well as a set
;; we use the set to determine if an element has occured
;; we use a vector to maintain the order of the occurrence
;; we can still use reduce to aggregate information (set and vec)
;; the aggregator itself would be a vector of set and vec.
(
  #(-> (fn [[v s] x]
         (if (s x)
           [v s]
           [(conj v x) (conj s x)]))
       (reduce [[] #{}] %)
       (first))
  [1 2 1 3 1 2 4])

;; 54 partition a sequence without using partition and partition-all
(defn my-partition
  ([n coll]
   (my-partition [] n coll))
  ([res n coll]
   (if (< (count coll) n)
     (apply list res)
     (recur (conj res (take n coll))
            n
            (drop n coll)))))

(my-partition 3 (range 9))

;; 60 sequence reductions without reductions
(defn my-reductions
  ([f coll]
   (my-reductions f (first coll) (next coll)))
  ([f e coll]
   (lazy-seq
     (cons e
      (if (seq coll)
        (my-reductions f (f e (first coll)) (rest coll)))))))

(take 5 (my-reductions + (range)))
(take 4 (my-reductions conj [1] [2 3 4]))

;; take away from this problem is to be careful about the order of laze-seq, cons and conditionals

;; 132 Insert between two items
;; if we need to process every pair of a collection, we can partition it into pair first
;; partition-all may be helpful for special cases (last element)
;; put expression (f a b) to last of the (and ...) clause so that if a or b is nill, this expression won't be evaluated
(defn insert
  [f value coll]
  (->> (partition-all 2 1 coll)
       (mapcat (fn [[a b]]
                 (if (and a b (f a b))
                   [a value]
                   [a])))))

(insert < :less [1 6 7 4 3])

(partition 2 1 [1 6 7 4 3])
;;=> ((1 6) (6 7) (7 4) (4 3))

(partition-all 2 1 [1 6 7 4 3])
;;=> ((1 6) (6 7) (7 4) (4 3) (3))