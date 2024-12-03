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

;; Part 2

(defn instructions [s]
  (->> (re-seq #"(mul\(\d+,\d+\))|(do\(\))|(don't\(\))" s)
       (map first)))

(instructions puzzle-input)

(defn parse-instruction [s]
  (str/split s #"\(|\)|,"))

(->> puzzle-input instructions
     (map parse-instruction)
     (reduce (fn [[state x] [code par1 par2]]
               (println state x code par1 par2)
               (cond
                 (= code "do")
                 [:do x]

                 (= code "don't")
                 [:don't x]

                 (and (= state :do)
                      (= code "mul"))
                 [state (+ x (* (parse-long par1)
                                (parse-long par2)))]

                 :else
                 [state x])) [:do 0])); [:don't 70478672]
