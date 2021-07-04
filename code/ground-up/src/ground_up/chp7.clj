(ns ground-up.chp7
  (:require [cheshire.core :refer :all]))
;; code and tests
(defn pow
  "Raises base to the given power"
  [base power]
  (apply * (repeat power base)))

;; Exploring data
(parse-string "{\"foo\":\"bar\"}" true)

(-> "2008.json" slurp parse-string first)

(def data (parse-string (slurp "2008.json") true))

(->> data (map :driving_under_influence) (apply max))

(->> data (map :driving_under_influence) sort (take-last 10))

;; `key` returns the key of the map entry
(->> data
     (map :driving_under_influence)
     frequencies
     (sort-by key)
     (take-last 10))


(->> data
     (map :driving_under_influence)
     frequencies
     (sort-by val)
     (take-last 10))

;; `keys` returns a sequence of the keys of a map instance
(->> data (sort-by :driving_under_influence)
     (take-last 2)
     (mapcat keys)
     (into (sorted-set))
     )

(->> data
     (sort-by :driving_under_influence)
     (take-last 2)
     (map #(select-keys % [:driving_under_influence :fips_county_code :fips_state_code])))

;(def fips (parse-string (slurp "FipsCountyCodes.json") true))
;
;(keys fips)
;
;(keys (:table fips))
;
;(->> fips :table :columnNames)

(def fips
  (->> (parse-string (slurp "FipsCountyCodes.json") true)
       :table :rows (into {})))

(fips "02000")

;; now we are refactoring the code
(defn load-json
  [file]
  (parse-string (slurp file) true))

(def fips
  (->> "FipsCountyCodes.json"
       load-json
       :table
       :rows
       (into {})))
;; one entry would be "39127" "OH, Perry"

(defn fips-code
  [county]
  (str (:fips_state_code county) (:fips_county_code county)))

(defn most-duis
  [file]
  (->> file
       load-json
       (sort-by :driving_under_influence)
       (take-last 10)
       (map (fn [county]
                 [(fips (fips-code county)) (:driving_under_influence county)]))
       (into {})))

(most-duis "2008.json")

;; Ex 1.
(defn prevalence-duis
  [file]
  (->> file
       load-json
       (map (fn [county]
              (if (= 0 (:county_population county))
                [(fips (fips-code county)) 0]
                [(fips (fips-code county)) (float (/ (:driving_under_influence county)
                                                  (:county_population county)))]
                )))
       (into {})
       (sort-by val)
       (take-last 10)))

(prevalence-duis "2008.json")

;; Ex 2.
(defn extended-duis
  [file]
  (->> file
       load-json
       (map (fn [county]
              [(fips (fips-code county))
               (if (= 0 (:county_population county))
                 0
                 (float (/ (:driving_under_influence county)
                           (:county_population county))))
               (:driving_under_influence county)
               (:county_population county)]
              ))
       (sort-by second)
       (take-last 10)))

(extended-duis "2008.json")

;; Ex 3.
(defn most-prevalent [file field]
  (->> file
       load-json
       (map (fn [county]
              [(fips (fips-code county))
               (if (= 0 (:county_population county))
                 0
                 (float (/ (field county)
                           (:county_population county))))
               ]
              ))
       (sort-by second)
       (take-last 10)))

(most-prevalent "2008.json" :arson)

;; Ex 4.
;; How to write a test to verify that most-prevalent is correct?
;; The test must rely on a test file



