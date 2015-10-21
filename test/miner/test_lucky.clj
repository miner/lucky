(ns miner.test-lucky
  (:require [clojure.test :refer :all]
            [miner.lucky :refer :all]))


(def lucky-100 [1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
               87, 93, 99])


(def lucky-300 [1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
               87, 93, 99, 105, 111, 115, 127, 129, 133, 135, 141, 151, 159, 163, 169, 171,
               189, 193, 195, 201, 205, 211, 219, 223, 231, 235, 237, 241, 259, 261, 267,
               273, 283, 285, 289, 297])


(deftest feeling-lucky
  (is (= (lucky-avl 100) lucky-100))
  (is (= (lucky-avl 300) lucky-300))
  (is (= (lucky-slow 300) lucky-300))
  (is (= (lucky3 300) lucky-300))
  (is (= (lucky5t 300) lucky-300)))
