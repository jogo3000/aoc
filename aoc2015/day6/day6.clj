(ns day6
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "day6/input.txt"))

(def turn-on "turn on")
(def turn-off "turn off")
(def toggle "toggle")


(defn instruction [row]
  (str/trim (re-find #"t[^\d]+" row)))

(defn coordinates [row]
  (->> row
       (re-find #"(\d+)(?:,)(\d+) through (\d+)(?:,)(\d+)")
       rest
       (map parse-long)))

(defn parse-input [input]
  (->> input
       str/trim
       str/split-lines
       (map (juxt instruction coordinates))))

(def grid {})

(defn run-instructions [instructions]
  (reduce (fn [grid [instruction [x1 y1 x2 y2]]]
            (let [f
                  (condp = instruction
                    turn-off (constantly false)
                    turn-on (constantly true)
                    toggle not
                    identity)]
              (if (= identity f)
                grid
                (reduce (fn [grid [x y]]
                          (update grid [x y] f))
                        grid
                        (for [x (range x1 (inc x2))
                              y (range y1 (inc y2))]
                          [x y])))))
          grid
          instructions))

(defn evaluate-result [grid]
  (count (filter second grid)))

(evaluate-result
 (run-instructions
  (parse-input "turn on 0,0 through 999,999
toggle 0,0 through 999,0
turn off 499,499 through 500,500")))

(->> puzzle-input
     parse-input
     run-instructions
     (filter second)
     count) ;; 400410


(defn run-ancient-nordic-elvish-instructions [instructions]
  (reduce (fn [grid [instruction [x1 y1 x2 y2]]]
            (let [f
                  (condp = instruction
                    turn-off (fn [lux] (max 0 (dec (or lux 0))))
                    turn-on (fn [lux] (inc (or lux 0)))
                    toggle (fn [lux] (+ (or lux 0) 2))
                    identity)]
              (if (= identity f)
                grid
                (reduce (fn [grid [x y]]
                          (update grid [x y] f))
                        grid
                        (for [x (range x1 (inc x2))
                              y (range y1 (inc y2))]
                          [x y])))))
          grid
          instructions))

(defn evaluate-result-for-brightness [grid]
  (->> grid vals (reduce +)))

(->> puzzle-input
     parse-input
     run-ancient-nordic-elvish-instructions
     evaluate-result-for-brightness) ; 15343601
