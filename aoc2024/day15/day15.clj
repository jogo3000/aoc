(ns day15
  (:require [clojure.string :as str]))

(def tiny-map
  "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
")

(def small-map (slurp "day15/small-map"))

(def puzzle-map (slurp "day15/input"))

(def robot \@)
(def wall \#)
(def box \O)
(def space \.)

(defn parse-input [input]
  (let [[game-map moves] (str/split input #"\n\n")
        m (->> game-map str/split-lines (mapv vec))
        moves (-> moves (str/replace #"\s" "") vec)]
    {:game-map m
     :moves moves}))

(defn find-tiletype [tiletype m]
  (for [y (range (count m))
        x (range (count (first m)))
        :let [tile (get-in m [y x])]
        :when (= tiletype tile)]
    [y x]))

(defn find-robot [m]
  (first
   (find-tiletype robot m)))

(defn find-walls [m]
  (find-tiletype wall m))

(defn find-boxes [m]
  (find-tiletype box m))

(defn left [[y x]]
  [y (dec x)])

(defn right [[y x]]
  [y (inc x)])

(defn up [[y x]]
  [(dec y) x])

(defn down [[y x]]
  [(inc y) x])

(defn simulate [m loc move]
  (let [this-tile (get-in m loc)
        tile-coord (move loc)
        tile (get-in m tile-coord)]
    (condp = tile
      wall m
      box (let [new-state (simulate m tile-coord move)]
            (if (= new-state m) m
                (-> new-state
                    (assoc-in loc space)
                    (assoc-in tile-coord this-tile))))
      space (-> m
                (assoc-in loc space)
                (assoc-in tile-coord this-tile))
      (throw (Exception. (str "no: " tile))))))

(defn render-map [m]
  (->> m
       (map str/join)
       (str/join \newline)))

(def parse-move {\< left \> right \^ up \v down})

(defn run-full-simulation [input]
  (let [{m :game-map moves :moves} (parse-input input)]
    (->> moves
         (reduce (fn [m move]
                   (let [loc (find-robot m)]
                     (simulate m loc (parse-move move)))) m))))

(defn evaluate-map [m]
  (->> (find-boxes m)
       (map (fn [[y x]]
              (+ x (* 100 y))))
       (apply +)))

(defn solve-pt1 [input]
  (->> input
       run-full-simulation
       evaluate-map))


(solve-pt1 tiny-map) ; 2028
(solve-pt1 small-map) ; 10092

(solve-pt1 puzzle-map) ; 1413675
