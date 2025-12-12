(ns day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
")

(defn square [x]
  (Math/pow x 2))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (square (- x1 x2)) (square (- y1 y2)) (square (- z1 z2)))))

(defn parse-input [input]
  (->> input
       str/trim
       str/split-lines
       (map #(->> (str/split % #",") (map parse-long)))))

(defn find-closest-box [boxes box]
  (->> boxes
       (remove (partial = box))
       (apply min-key (partial distance box))))

(let [boxes (parse-input sample)
      closest-pairs
      (->> boxes
           (map (juxt identity (partial find-closest-box boxes)))
           (reduce (fn [acc [b1 b2]]
                     (assoc acc #{b1 b2} (distance b1 b2))) {})
           (map (fn [[pair d]] {:distance d
                                :pair pair}))
           (sort-by :distance))]
  [closest-pairs
   (->> closest-pairs
        (reduce (fn [acc {pair :pair}]
                  (let [b1 (first pair) b2 (second pair)
                        circuit (or (acc b1)
                                    (acc b2)
                                    (random-uuid))]
                    (assoc acc b1 circuit b2 circuit))) {})
        (reduce (fn [acc [box circuit]]
                  (update acc circuit conj box)) {})
        (sort-by (comp count second))
        reverse
        (take 3)
        (map (comp count second))
        (reduce *))]) ; 729 <- too low?

#_(let [boxes (parse-input (slurp "day8/input"))
        connected-boxes (connect-closest-boxes boxes)]
    (->> connected-boxes
         (reduce (fn [acc [box circuit]]
                   (update acc circuit conj box)) {})
         (sort-by (comp count second))
         reverse
         (take 3)
         (map (comp count second))
         (reduce *)))

(defn all-possible-connections [boxes]
  (for [b1 boxes
        b2 boxes
        :when (not= b1 b2)]
    {:d (distance b1 b2)
     :pair
     #{b1 b2}}))

(defn find-matching-circuits [circuits pair]
  (->> (filter (fn [circuit]
                 (when (seq (set/intersection circuit pair))
                   circuit)) circuits)
       (into #{})))

(let [boxes (parse-input (slurp "day8/input"))
      possible-connections
      (all-possible-connections boxes)
      shortest-to-longest (->> possible-connections distinct
                               (sort-by :d))]
  (->> shortest-to-longest
       (take 1000)
       (reduce (fn [circuits {:keys [pair]}]
                 (let [matches (find-matching-circuits circuits pair)]
                   (-> circuits
                       (set/difference matches)
                       (conj (into (apply set/union matches) pair)))))
               (into #{}
                     (map (comp set vector))
                     boxes))
       (sort-by count)
       reverse
       (take 3)
       (map count)
       (reduce *))) ; 181584

;; Part 2

(let [boxes (parse-input (slurp "day8/input"))
      possible-connections
      (all-possible-connections boxes)
      shortest-to-longest (->> possible-connections distinct
                               (sort-by :d))]
  (->> shortest-to-longest
       (reduce (fn [circuits {:keys [pair]}]
                 (let [matches (find-matching-circuits circuits pair)
                       new-circuits
                       (-> circuits
                           (set/difference matches)
                           (conj (into (apply set/union matches) pair)))]
                   (if (= 1 (count new-circuits))
                     (reduced pair)
                     new-circuits)))
               (into #{}
                     (map (comp set vector))
                     boxes))
       (map first)
       (reduce *))) ; 8465902405
