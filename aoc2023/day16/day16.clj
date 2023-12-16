(ns day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set! *warn-on-reflection* true)

(def sample (str/trim (slurp "/home/jogo3000/git/aoc2022/aoc2023/day16/sample.txt")))
(def puzzle-input (str/trim (slurp "/home/jogo3000/git/aoc2022/aoc2023/day16/input.txt")))

(defn parse-input [s]
  (->> s
       str/split-lines
       (mapv vec)))

(defn left [[y x]]
  [y (dec x)])

(defn right [[y x]]
  [y (inc x)])

(defn up [[y x]]
  [(dec y) x])

(defn down [[y x]]
  [(inc y) x])

(defn shoot-beams [m [dir pos]]
  (let [height (count m)
        width (count (first m))
        tile (get-in m pos)
        candidate-beams
        (case tile
          \. ; floor
          [[dir (dir pos)]]

          \\ ; mirror \
          (cond
            (= dir right) [[down (down pos)]]
            (= dir left) [[up (up pos)]]
            (= dir up) [[left (left pos)]]
            (= dir down) [[right (right pos)]])

          \/ ; mirror /
          (cond
            (= dir right) [[up (up pos)]]
            (= dir left) [[down (down pos)]]
            (= dir up) [[right (right pos)]]
            (= dir down) [[left (left pos)]])

          \| ; beam splitter |
          (cond
            (= dir up) [[up (up pos)]]
            (= dir down) [[down (down pos)]]
            (= dir left) [[up (up pos)]
                          [down (down pos)]]
            (= dir right) [[up (up pos)]
                           [down (down pos)]])

          \- ; beam splitter -
          (cond
            (= dir right) [[right (right pos)]]
            (= dir left) [[left (left pos)]]
            (= dir up) [[left (left pos)]
                        [right (right pos)]]
            (= dir down) [[left (left pos)]
                          [right (right pos)]]))]

    (->> candidate-beams
         (remove (fn [[_ pos']]
                   (or (some neg? pos')
                       (>= (first pos') height)
                       (>= (second pos') width)))))))
(defrecord Beam [head visited])

(defn energize-tiles [m]
  (loop [n 0
         done-beams #{}
         beams [(->Beam [right [0 0]] #{})]
         energized {[0 0] #{right}}]
    (if (empty? beams) energized
        (let [beams' (mapcat (fn [beam] (let [new-beam-heads (shoot-beams m (:head beam))]
                                          (map
                                           (fn [beam-head]
                                             (->Beam beam-head (conj (:visited beam)
                                                                     (:head beam))))
                                           new-beam-heads))) beams)
              done-beams' (set (filter (fn [beam] ((:visited beam) (:head beam))) beams'))
              energized' (merge-with
                          into
                          energized
                          (into {}
                                (map (fn [beam]
                                       (let [[dir pos] (:head beam)]
                                         [pos #{dir}])))
                                beams'))]
          (recur (inc n)
                 (into done-beams done-beams')
                 (remove done-beams' beams')
                 energized')))))

(defn count-energized-tiles [input]
  (let [m (parse-input input)]
    (count (energize-tiles m))))

(count-energized-tiles sample)

(defn visualize [m ts]
  (let [height (count m)
        width (count (first m))]
    (str/join
     \newline
     (for [y (range height)]
       (str/join
        (for [x (range width)]
          (let [tile (get-in m [y x])]
            (if (not= \. tile)
              tile
              (if (contains? ts [y x])
                \#
                tile)))))))))

(count-energized-tiles puzzle-input)
;; 7199
