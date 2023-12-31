(ns day9
  (:require [clojure.string :as str]))

;; Traveling salesman problem with 28 cities, brute force is not going to fly.
;; Well actually, there aren't edges between all towns. Perhaps it can be brute
;; forced after all.
(def puzzle-input (slurp "day9/input.txt"))

(defn parse-line [s]
  (let [[a b c] (str/split s #"( to )|( = )")]
    [[a b] (parse-long c)]))

(defn parse-input [s]
  (->> s
       str/trim
       str/split-lines
       (map parse-line)
       (into {})))

(def distance-matrix (parse-input puzzle-input))
(def distances-both-directions (into distance-matrix
                                     (map (fn [[[a b] d]]
                                            [[b a] d]) distance-matrix)))

(def locations (->> distance-matrix keys flatten set))

(count locations) ;; 8 So it's actually only 8 cities

;; Possible moves would be list of distances * 2

(def possible-moves (into (keys distance-matrix)
                          (->> distance-matrix keys (map (comp vec reverse)))))

(count possible-moves); 56, but this list already contains possibilities of
                      ; going through cities twice!

(defn travelling-santa [travelling-santa visited path distance]
  (if (= (set visited) locations)
    distance
    (let [here (first path)
          possible-moves
          (->> possible-moves
               (filter (fn [[from to]]
                         (when-not (contains? visited to)
                           (= from here)))))]
      (reduce (fn [acc [from to]]
                (min acc (travelling-santa
                          travelling-santa
                          (conj visited to)
                          (cons to path)
                          (+ distance (get distances-both-directions [from to])))))
              Long/MAX_VALUE
              possible-moves))))

(def mem-travelling-santa (memoize travelling-santa))

(for [loc locations]
  (mem-travelling-santa mem-travelling-santa #{loc} (list loc) 0))

;; Shortest route is 207

;; Part deux is longest route
(defn scenic-route-travelling-santa [travelling-santa visited path distance]
  (if (= (set visited) locations)
    distance
    (let [here (first path)
          possible-moves
          (->> possible-moves
               (filter (fn [[from to]]
                         (when-not (contains? visited to)
                           (= from here)))))]
      (reduce (fn [acc [from to]]
                (max acc (travelling-santa
                          travelling-santa
                          (conj visited to)
                          (cons to path)
                          (+ distance (get distances-both-directions [from to])))))
              Long/MIN_VALUE
              possible-moves))))

(def mem-scenic (memoize scenic-route-travelling-santa))

(for [loc locations]
  (mem-scenic mem-scenic #{loc} (list loc) 0))

;; Longest is 804
