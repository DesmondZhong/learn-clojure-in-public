(ns ground-up.chp7-test
  (:require [clojure.test :refer :all]
            [ground-up.chp7 :refer :all]))

(deftest pow-test
  (testing "unity"
    (is (= 1 (pow 1 1))))
  (testing "square integers"
    (is (= 9 (pow 3 2))))
  (testing "0^0"
    (is (= 1 (pow 0 0)))))

(deftest fips-code-test
  (testing "fips-code-concat"
    (is (= "12345" (fips-code {:fips_state_code "123"
                               :fips_county_code 45})))))