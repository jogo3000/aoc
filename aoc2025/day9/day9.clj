(ns day9
  (:require [clojure.string :as str]))

(def sample "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
")

(defn parse-input [input]
  (->> input str/trim str/split-lines (map (fn [s] (->> (str/split s #",") (map parse-long))))))

(defn m-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn furthest-pair [tiles]
  (->> (for [t1 tiles
             t2 tiles
             :when (not= t1 t2)]
         (list (m-distance t1 t2) t1 t2))
       (reduce (fn [best t?]
                 (if (> (first t?) (first best)) t? best)))
       (drop 1)))

(defn area [[x1 y1] [x2 y2]]
  (* (inc (abs (- x1 x2)))
     (inc (abs (- y1 y2)))))

(let [tiles (parse-input (slurp "day9/input"))
      [p1 p2] (furthest-pair tiles)]
  (area p1 p2)) ; 4743645488

;; Part 2

(defn exterior [points]
  (->> points
       cycle
       (take (inc (count points)))
       (partition 2 1)
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (let [xdots (cond-> (range (min x1 x2) (inc (max x1 x2)))
                               (> x1 x2) reverse)
                       ydots (cond-> (range (min y1 y2) (inc (max y1 y2)))
                               (> y1 y2) reverse)]
                   (take (max (count xdots) (count ydots))
                         (map vector (cycle xdots) (cycle ydots))))))))

(defn min-key* [f points]
  (f (reduce (fn [best point] (min-key f best point)) points)))

(defn max-key* [f points]
  (f (reduce (fn [best point] (max-key f best point)) points)))

(defn min-x [points]
  (min-key* first points))

(defn min-y [points]
  (min-key* second points))

(defn max-x [points]
  (max-key* first points ))

(defn max-y [points]
  (max-key* second points))

(defn all-pairs [points]
  (->> (for [t1 points
             t2 points
             :when (not= t1 t2)]
         (list (m-distance t1 t2) #{t1 t2}))
       (into #{})))

;; Too slow, but works
#_
(let [red-tiles (parse-input (slurp "day9/input"))
      border (exterior red-tiles)
      max-x-border (max-x red-tiles)
      border-points (into #{} border)
      all-pairs-best-first  (->> (all-pairs red-tiles)
                                 (sort-by first)
                                 reverse)]
  (->> all-pairs-best-first
       (some (fn [[_ pair]]
               (let [rect-exterior (exterior [[(min-x pair) (min-y pair)]
                                              [(min-x pair) (max-y pair)]
                                              [(max-x pair) (max-y pair)]
                                              [(max-x pair) (min-y pair)]])
                     to-be-tested (into #{}
                                        (remove border-points)
                                        rect-exterior)]
                 (when (->> to-be-tested
                            (every? (fn [[x y]]
                                      (->>
                                       (reduce (fn [{:keys [intersections on-edge] :as state} x']
                                                 (cond
                                                   (and (not on-edge)
                                                        (border-points [x' y]))
                                                   {:intersections (inc intersections)
                                                    :on-edge true}

                                                   (and on-edge
                                                        (not (border-points [x' y])))
                                                   {:intersections intersections
                                                    :on-edge false}

                                                   :else state))
                                               {:intersections 0
                                                :on-edge false} (range x (inc max-x-border)))
                                       :intersections
                                       odd?))))
                   pair))))))
