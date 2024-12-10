(ns day10
  (:require [clojure.string :as str]))

(def sample-map
  "0123
1234
8765
9876")

(def larger-sample
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
")


(map (comp (partial + -48) int) "12344")

(defn parse-input [input]
  (->> input str/split-lines
       (mapv #(mapv (comp (partial + -48) int) %))))

(defn north [[x y]]
  [(dec x) y])

(defn south [[x y]]
  [(inc x) y])

(defn west [[x y]]
  [x (dec y)])

(defn east [[x y]]
  [x (inc y)])

(defn find-starting-positions [m]
  (let [max-x (count m)
        max-y (count (first m))]
    (vec
     (for [x (range max-x)
           y (range max-y)
           :let [tile (get-in m [x y])]
           :when (zero? tile)]
       [[x y]]))))


(defn find-trails [input]
  (let [m (parse-input input)
        starting-positions
        (find-starting-positions m)]
    (->>
     (loop [cursors starting-positions]
       (if (every? #(= 9 (get-in m (peek %))) cursors) cursors
           (recur
            (->> cursors
                 (reduce (fn [acc cursor]
                           (let [pos (peek cursor)
                                 tile (get-in m pos)]
                             (if (= tile 9) (into acc cursor)
                                 (into acc
                                       (comp
                                        (filter #(= (get-in m %) (inc tile)))
                                        (map #(conj cursor %)))
                                       [(north pos)
                                        (south pos)
                                        (east pos)
                                        (west pos)])))) []))))))))

(defn evaluate-paths-pt1 [paths]
  (->> paths (map (juxt first peek)) (into #{}) count))

(->> larger-sample
     find-trails
     evaluate-paths-pt1)

(->> (slurp "day10/input")
     find-trails
     evaluate-paths-pt1) ; 667

(defn evaluate-paths-pt2 [paths]
  (->> paths distinct count))

(->> larger-sample
     find-trails
     evaluate-paths-pt2)

(->> (slurp "day10/input")
     find-trails
     evaluate-paths-pt2) ; 1344
