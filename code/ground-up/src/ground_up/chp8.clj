(ns ground-up.chp8
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [pst]]))

(defn atlas-v
  [next-stage]
  {:dry-mass  50050
   :fuel-mass 284450
   :time 0
   :isp  3050
   :max-fuel-rate (/ 284450 253)
   :max-thrust 4.152e6
   :next-stage next-stage})

(defn mass
  "The total mass of a craft"
  [craft]
  (+ (:dry-mass craft) (:fuel-mass craft)))

(def earth-equatorial-radius
  "Radius of the earth, in meters"
  6378137)

(def earth-day
  "Length of an earth day in seconds."
  86400)

(def earth-equatorial-speed
  "How fast points on th equator move, relative to the center of the earth,
   in meters/sec"
  (/ (* 2 Math/PI earth-equatorial-radius)
     earth-day))

(def initial-space-center
  "The initial position and velocity of the launch facility"
  {:time     0
   :position {:x earth-equatorial-radius
              :y 0
              :z 0}
   :velocity {:x 0
              :y earth-equatorial-speed
              :z 0}})

(defn prepare
  "Prepare a craft for launch from an equatorial space center."
  [craft]
  (merge craft initial-space-center))

;; Forces
(defn magnitude
  "the radius given the cartesian coordinates"
  [c]
  (Math/sqrt (+ (Math/pow (:x c) 2)
                (Math/pow (:y c) 2)
                (Math/pow (:z c) 2))))

(defn cartesian->spherical
  ""
  [c]
  (let [r (magnitude c)]
    {:r r
     :phi   (Math/acos (/ (:z c) r))
     :theta (Math/atan (/ (:y c) (:x c)))}))


;; Note, the function on the website has a typo
(defn spherical->cartesian
  [c]
  {:x (* (:r c) (Math/sin (:phi c)) (Math/cos (:theta c)))
   :y (* (:r c) (Math/sin (:phi c)) (Math/sin (:theta c)))
   :z (* (:r c) (Math/cos (:phi c)))})

(def g -9.8)

(defn gravity-force
  [craft]
  (let [total-force (* g (mass craft))]
    (-> craft
        :position
        cartesian->spherical
        (assoc :r total-force)                              ; this is smart
        spherical->cartesian)))

(declare ascent)
(declare circulation)
(defn fuel-rate
  "How fast if the fuel, in kg/s, consumed by the craft?"
  [craft]
  (cond
    ;; out of fuel
    (<= (:fuel-mass craft) 0)
    0
    ;; Ascent burn
    (<= (first ascent) (:time craft) (last ascent))
    (:max-fuel-rate craft)
    ;; circulation burn
    (<= (first circulation) (:time craft) (last circulation))
    (:max-fuel-rate craft)
    :else 0))

(defn thrust
  "How much force, in neurons, does the craft's rocket engines exert?"
  [craft]
  (* (fuel-rate craft) (:isp craft)))

(declare unit-vector)
(declare scale)
(declare orientation)
(defn engine-force
  "The force vector, pointing to the x axis"
  [craft]
  (scale (thrust craft) (unit-vector (orientation craft))))

(defn total-force
  "sums up gravity and engine forces"
  [craft]
  (merge-with + (gravity-force craft)
                (engine-force craft)))

(defn map-values
  "Applies f to every value in the map m"
  [f m]
  (into {}
        (map (fn [pair]
               [(key pair) (f (val pair))])
             m)))

(defn scale
  "Multiplies the value of a map by the given factor"
  [factor coordinates]
  (map-values (partial * factor) coordinates))

(defn acceleration
  "Total acceleration of a craft"
  [craft]
  (scale (/ 1 (mass craft)) (total-force craft)))

(declare stage)
(defn step
  [craft dt]
  (let [craft (stage craft)]
    (assoc craft
      :time (+ dt (:time craft))
      :fuel-mass (- (:fuel-mass craft) (* dt (fuel-rate craft)))
      :position (merge-with + (:position craft)
                            (scale dt (:velocity craft)))
      :velocity (merge-with + (:velocity craft)
                            (scale dt (acceleration craft))))))

;;; Launch
;(-> (atlas-v)
;    prepare
;    pprint)
;
;(-> (atlas-v)
;    prepare
;    (step 1)
;    pprint)

;; (pst *e)

;; Flight
(defn trajectory
  [dt craft]
  (iterate #(step % 1) craft))

;(->>
;  (atlas-v)
;  prepare
;  (trajectory 1)
;  (take 3)
;  pprint)

(defn altitude
  "The height above the surface of the equator, in meters"
  [craft]
  (-> craft
      :position
      cartesian->spherical
      :r
      (- earth-equatorial-radius)))

;(->>
;  (atlas-v)
;  prepare
;  (trajectory 1)
;  (map altitude)
;  (take 10)
;  pprint)

(defn above-ground?
  "Is the craft at or above the surface?"
  [craft]
  (<= 0 (altitude craft)))

(defn flight
  "The above ground portion of a trajectory"
  [trajectory]
  (take-while above-ground? trajectory))

(defn crashed?
  "Does this trajectory crash into the surface before 100 hours are up?"
  [trajectory]
  (let [time-limit (* 10 3600)]
    (not (every? above-ground?
                 (take-while #(<= (:time %) time-limit)
                             trajectory)))))

(defn crash-time
  "Given a trajectory, returns the time the rocket impacted the ground."
  [trajectory]
  (:time (last (flight trajectory))))

(defn apoapsis
  "The highest altitude achieved during a trajecotry"
  [trajectory]
  (apply max (map altitude (flight trajectory))))

(defn apoapsis-time
  "The time of apoapsis"
  [trajectory]
  (:time (apply max-key altitude (flight trajectory))))

;; Stage II
(defn centaur
  []
  {:dry-mass 2361
   :fuel-mass 13897
   :isp 4354
   :max-fuel-rate (/ 13897 470)})

(defn stage
  [craft]
  (cond
    ;; still fuel left
    (pos? (:fuel-mass craft))
    craft
    ;; No remaining stages
    (nil? (:next-stage craft))
    craft
    ;; Stage!
    :else
    (merge (:next-stage craft)
           (select-keys craft [:time :position :velocity]))
    ))

(defn unit-vector
  "Scale coordinates to magnitude 1."
  [coordinates]
  (scale (/ (magnitude coordinates)) coordinates))

(def ascent "The start and end times for the ascent burn."
  [0 300])

(def circulation
  "The start and end times for the circularization burn"
  [400 1000])

(defn dot-product
  [c1 c2]
  (+ (* (:x c1) (:x c2))
     (* (:y c1) (:y c2))
     (* (:z c1) (:z c2))))

(defn projection
  [a b]
  (let [b (unit-vector b)]
    (scale (dot-product a b) b)))

(defn rejection
  [a b]
  (let [a' (projection a b)]
    {:x (- (:x a) (:x a'))
     :y (- (:y a) (:y a'))
     :z (- (:z a) (:z a'))}))

(defn orientation
  [craft]
  (cond
    (<= (first ascent) (:time craft) (last ascent))
    (:position craft)

    (<= (first circulation) (:time craft) (last circulation))
    (rejection (:velocity craft) (:position craft))

    :else
    (:velocity craft)))

