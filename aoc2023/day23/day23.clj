(ns day23
  (:require [clojure.string :as str]))

(def sample-input (slurp "day23/sample.txt"))
(def puzzle-input (slurp "day23/input.txt"))

(def path \.)
(def forest \#)
(def east-slope \>)
(def west-slope \<)
(def north-slope \^)
(def south-slope \v)

(defrecord Map [m max-y max-x])

(defn parse-input [input]
  (let [m (->> input
               str/trim
               str/split-lines
               (mapv vec))
        max-y (dec (count m))
        max-x (dec (count (first m)))]
    (->Map m max-y max-x)))

(defn start [m]
  [0 1])

(defn end [m]
  [(:max-y m) (dec (:max-x m))])

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(defn possible-moves [m visited pos]
  (into []
        (comp
         (map #(% pos))
         (filter #(<= 0 (first %) (:max-y m))) ;; don't cross borders vert
         (filter #(<= 0 (second %) (:max-y m))) ;; don't cross borders horizontally
         (remove #(= (get-in (:m m) %) forest)) ;; don't step in to the forest. Don't!
         (remove visited)) ;; Don't repeat your steps
        [north south east west]))

(def sample-map (parse-input sample-input))
