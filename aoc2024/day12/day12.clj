(ns day12
  (:require [clojure.string :as str]))

(def small-example
  "AAAA
BBCD
BBCC
EEEC
")

(def die-example
  "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
")

(def large-example
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
")

(defn parse-map [input]
  (->> input str/split-lines (mapv vec)))

(defn all-squares [m]
  (for [x (range (count m))
        y (range (count (first m)))]
    [x y]))

(defn map-region [m start]
  (let [kind (get-in m start)]
    (loop [[cursor & queue] [start]
           found #{}]
      (if (and (not cursor)
               (empty? queue)) found
          (if (not= kind (get-in m cursor)) (recur queue found)
              (recur (into queue
                           (comp
                            (map (fn [[a b]] [(+ (first cursor) a)
                                              (+ (second cursor) b)]))
                            (filter (complement found)))
                           [[1 0] [0 1] [0 -1] [-1 0]])
                     (into found [cursor])))))))

(defn map-regions [m]
  (let [all-squares (all-squares m)]
    (loop [visited-squares #{}
           regions #{}]
      (if (= (into #{} all-squares) visited-squares)
        regions
        (let [start (first (filter (complement visited-squares) all-squares))
              kind (get-in m start)
              region (map-region m start)]
          (recur (into visited-squares region)
                 (conj regions [kind region])))))))

(->> small-example
     parse-map
     map-regions)

#{[\D #{[1 3]}] [\E #{[3 0] [3 1] [3 2]}] [\A #{[0 0] [0 3] [0 2] [0 1]}] [\B #{[1 0] [1 1] [2 0] [2 1]}] [\C #{[2 2] [2 3] [3 3] [1 2]}]}

(defn find-interconnections [region]
  (let [tiles (vec region)]
    (apply concat
           (for [i (range (count tiles))]
             (for [j (range i (count tiles))
                   :let [[x1 y1] (nth tiles i)
                         [x2 y2] (nth tiles j)
                         dist [(abs (- x2 x1)) (abs (- y2 y1))]]
                   :when (= 1 (apply + dist))]
               [[x1 y1] [x2 y2]])))))


(defn find-perimeters [input]
  (->> input
       parse-map
       map-regions
       (map (fn [[kind region]]
              (let [area (count region)
                    connections (count (find-interconnections region))
                    perimeter (- (* 4 area) (* 2 connections))]
                {:kind kind
                 :area area
                 :connections connections
                 :perimeter perimeter
                 :price (* area perimeter)})))))

(find-perimeters small-example)
(find-perimeters die-example)

(->> (find-perimeters large-example)
     (map :price)
     (apply +))

(->> (slurp "day12/input")
     find-perimeters
     (map :price)
     (apply +)) ; 1473408
