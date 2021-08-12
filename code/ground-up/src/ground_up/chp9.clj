(ns ground-up.chp9
  (:require [clojure.repl :refer [pst]]))

(defn bake
  [pie temp time]
  (assoc pie :tastiness
             (condp < (* temp time)
               400 :burned
               350 :perfect
               300 :soggy)))

(bake {:flavor :blackberry} 375 10.25)

(pst)

(defn perimeter
  "Given a rectangle, returns a vector of its edge lengths."
  [rect]
  [(:x rect)
   (:y rect)
   (:x rect)
   (:y rect)])

(defn frame
  "Given a mat width, and a photo rectangle, figure out the size of
  the frame required by adding the mat width around all edges of the photo"
  [mat-width rect]
  (let [margin (* 2 mat-width)]
    {:x (+ margin (:x rect))
     :y (+ margin (:y rect))}))

(def failure-rate 1/8)

(defn spares
  "Given a list of segments, figure out roughly how many of each distinct
  size would go bad, and emit a sequence of spare segments, assuming we screw up
  'failure-rate' of them."
  [segments]
  (->> segments
       frequencies
       (mapcat (fn [[segment n]]
                 (repeat (* failure-rate n)
                         segment)))))

(def cut-size 1)

(defn total-wood
  [mat-width photos]
  (let [segments (->> photos
                      (map (partial frame mat-width))
                      (mapcat perimeter))]
    (->> segments
         (concat (spares segments))
         (interpose cut-size)
         (reduce +))))

(->> [{:x 8
       :y 10}
      {:x 10
       :y 8}
      {:x 20
       :y 30}]
     (total-wood 2)
     (println "total inches:"))

(pst)

(.printStackTrace *e)