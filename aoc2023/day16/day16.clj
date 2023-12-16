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

(defn traverse-until-stop-or-mirror [m beam]
  (loop [beam beam
         path [beam]]
    (let [b' (shoot-beams m beam)]
      (if (and (= \. (get-in m (second beam)))
               (= 1 (count b')))
        (recur (first b')
               (conj path (first b')))
        [path b']))))

(def traverse (memoize traverse-until-stop-or-mirror))

(defn energize-tiles [m taken-paths beam]
  (let [[this-path new-beams] (traverse m beam)
        taken' (conj taken-paths (first this-path))]

    (reduce (fn [{:keys [path taken] :as all} beam]
              (let [recur-result (energize-tiles m taken beam)]
                {:path (into path
                             (:path recur-result))
                 :taken (into taken (:taken recur-result))}))
            {:path this-path
             :taken taken'}
            (remove taken-paths new-beams))))

(energize-tiles (parse-input sample) #{} [right [0 0]])

(defn count-energized-tiles [input]
  (println "----- new round ------")
  (let [m (parse-input input)]
    (count (set (map second (:path (energize-tiles m #{} [right [0 0]])))))))

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
