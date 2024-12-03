(ns day3
  (:require [clojure.string :as str]))

(def sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def puzzle-input (slurp "day3/input"))

(defn muls [s]
  (re-seq #"mul\(\d+,\d+\)" s))


(defn parse-mul [s]
  (map parse-long (re-seq #"\d+" s)))

(defn solve-part1 [input]
  (->> input
       muls
       (map parse-mul)
       (map #(apply * %))
       (apply +)))

(solve-part1 sample-input) ; 161

(solve-part1 puzzle-input); 164730528
