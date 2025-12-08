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

(let [input (slurp "day6/input")
      longest-line (->> input str/trim str/split-lines (mapv vec) (map count) (apply max))]
  (->> input
       str/trim
       str/split-lines
       (mapv vec)
       (mapv (fn [line] (if (< (count line) longest-line) (into line (repeat (- longest-line (count line)) \space)) line)))
       (apply map list)
       (partition-by (fn [coll]
                         (every? #(= \space %) coll)))
       (remove #(= 1 (count %)))
       (map (fn [cols]
                (let [operator (some #(#{\* \+} %) (apply concat cols))]
                  (->> cols
                       (map (fn [col]
                              (->> col
                                  (remove #(#{\* \+ \space} %))
                                  str/join
                                  parse-long)))
                       (reduce (case operator \* * \+ +))))))
       (reduce +))) ; 9640641878593
