(ns day6
  (:require [clojure.string :as str]))

(def sample
  "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
")

(defn parse-input [input]
  (->> input str/trim str/split-lines
       (mapv #(str/split (str/trim %) #"\s+"))
       (apply map list)
       (map (fn [xs]
              (cons (case (last xs)
                      "*" *
                      "+" +)
                    (->> xs butlast (map parse-long)))))))

(->> (parse-input (slurp "day6/input"))
     (map eval)
     (reduce +)) ; 6503327062445

;; Part 2
