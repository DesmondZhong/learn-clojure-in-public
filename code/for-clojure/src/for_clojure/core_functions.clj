(ns for-clojure.core-functions)

;;;;;;;;;;;;;;;;;; Easy ;;;;;;;;;;;;;;;;;;;;

;; 38 maximum value without max and max-key

(defn my-max
  [& args]
  (- (apply min (map - args))))

(my-max 1 8 3 4)

;; a more clojure way is to sort first and then take hte last one
;; notice how to specify the argument list in the anonoymous function
#(last (sort %&))

;; 61 Map construction without using zipmap
(defn my-zipmap
  [k v]
  (into {} (map vec (partition 2 (interleave k v)))))

;; draft
(my-zipmap [:a :b :c] [1 2 3])

(into {} (map vec (partition 2 (interleave [:a :b :c] [1 2 3]))))

;; an easier way is to directly construct a hash-map after interleave
#(apply hash-map (interleave %1 %2))

;; 63 group a sequence without using group-by
;; notice the use of list comprehension!
;; here we are concatenate the vector using into!
;; the key of the group-by operation is merge-with and into
;; here into can be replaced with concat

(defn my-group-by
  [f coll]
  (apply merge-with into  (for [v coll] {(f v) [v]})))

;; merge-with merges maps


;; we can also use update-in
;; notice how the special case of no value associated with the key is
;; handled by (or ...)
(defn my-group-by-2
  [f coll]
  (reduce #(update-in %1 [(f %2)] (fn [v] (conj (or v []) %2)))
          {}
          coll))

;; 69 Merge with a function without using merge-with
;; need to use nested reduce since we have to iterate keys in a map
(defn my-merge-with
  [f & maps]
  (reduce (fn [m1 m2]
            (reduce (fn [m [k v]]
                      (if (contains? m k)
                        (update-in m [k] f v)
                        (assoc m k v)))
                    m1 m2))
          maps))

;; another way to achieve it
(def maps '({:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))
;; notice that concat maps result in a list of key value pairs
(->> (apply concat maps))
;;=> ([:a 2] [:b 3] [:c 4] [:a 2] [:b 2] [:c 5])
(type (first (apply concat maps)))
;;=> clojure.lang.MapEntry
;; notice that this is a map entry! key value structure is still preserved.
(->> (apply concat maps)
     (group-by key))
(defn my-merge-with-2
  [f & maps]
 (->> (apply concat maps)
      (group-by key)
      (map (fn [k v]
             [k (reduce f (vals v))]))
      (into {})))

