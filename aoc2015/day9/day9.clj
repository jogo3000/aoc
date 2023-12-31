(ns day9
  (:require [clojure.string :as str]))

;; Traveling salesman problem with 28 cities, brute force is not going to fly.
;; Well actually, there aren't edges between all towns. Perhaps it can be brute
;; forced after all.
(def puzzle-input (slurp "day9/input.txt"))

(defn parse-line [s]
  (let [[a b c] (str/split s #"( to )|( = )")]
    [[a b] c]))

(defn parse-input [s]
  (->> s
       str/trim
       str/split-lines
       (map parse-line)
       (into {})))

(def distance-matrix (parse-input puzzle-input))

(def locations (->> distance-matrix keys flatten set))

(count locations) ;; 8 So it's actually only 8 cities

;; Possible moves would be list of distances * 2

(def possible-moves (into (keys distance-matrix)
                          (->> distance-matrix keys reverse)))

(count possible-moves); 56, but this list already contains possibilities of
                      ; going through cities twice!
