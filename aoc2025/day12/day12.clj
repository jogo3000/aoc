(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample (slurp "day12/sample"))

(defn parse-shape [s]
  (let [id (->> s first str parse-long)
        shape (->> (subs s 3) str/split-lines (mapv vec))]
    [id
     (into #{}
           (for [y (range (count shape))
                 x (range (count (first shape)))
                 :when (= \# (get-in shape [y x]))]
             [y x]))]))

(defn parse-region [s]
  (let [[size & counts] (str/split s #"\s")]
    {:size (->> (str/split size #"[x:]") (map parse-long))
     :counts (->> counts (map-indexed (fn [i s] [i (parse-long s)])) (into {}))}))

(defn parse-input [input]
  (let [segments (-> input str/trim (str/split #"\n\n"))
        shapes (->> segments butlast (map parse-shape) (into {}))
        regions (->> segments last str/split-lines (map parse-region))]
    [shapes regions]))

(defn min-x [points]
  (->> points (map second) (reduce min)))

(defn min-y [points]
  (->> points (map first) (reduce min)))

(defn draw-shape [points]
  (let [ys (map first points)
        xs (map second points)
        min-y (reduce min ys)
        max-y (reduce max ys)
        min-x (reduce min xs)
        max-x (reduce max xs)]
    (str/join
     "\n"
     (for [y (range min-y (inc max-y))]
       (str/join
        (for [x (range min-x (inc max-x))]
          (if (contains? points [y x]) \# \.)))))))

(defn rotate [shape]
  (let [theta (Math/toRadians 90)]
    (into #{}
          (comp
           (map #(map dec %))
           (map (fn [[y x]]
                  [(+ (* x (Math/sin theta))
                      (* y (Math/cos theta)))

                   (- (* x (Math/cos theta))
                      (* y (Math/sin theta)))]))
           (map #(mapv Math/round %))
           (map #(map inc %)))
          shape)))

(defn reflect [shape]
  (let [theta (Math/toRadians 180)]
    (into #{}
          (comp
           (map #(map dec %))
           (map (fn [[y x]]
                  [(- (* x (Math/sin theta))
                      (* y (Math/cos theta)))

                   (+ (* x (Math/cos theta))
                      (* y (Math/sin theta)))]))
           (map #(mapv Math/round %))
           (map #(map inc %)))
          shape)))

(defn possible-transformations [shape]
  (concat (take 4 (iterate rotate shape))
          (take 4 (iterate rotate (reflect shape)))))

(defn offset [shape [y x :as _point]]
  (into #{}
        (map (fn [[ys xs]]
               [(+ ys y) (+ x xs)]) shape)))

(defn fits? [space piece]
  (= (set/intersection space piece)
     piece))

(defn layout-initial-space [[y x]]
  (into #{}
        (for [y (range y)
              x (range x)]
          [y x])))

(defn try-insert [space shape point]
  (let [shape-at-point (offset shape point)]
    (when (fits? space shape-at-point)
      (set/difference space shape-at-point))))

(defn possible-inserts [space shape]
  (keep (partial try-insert space shape) space))

(defn can-fit? [shapes space pieces]
  (>= (count space)
      (->> pieces (map (comp count first shapes)) (reduce +))))

(defn my-mapcat
  "http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html"
  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (my-mapcat f (rest coll))))))

(defn fit [shapes space pieces]
  (if-not
      (seq pieces)
      space

      (when (can-fit? shapes space pieces)
        (let [transformations (->> pieces first shapes)
              remaining-spaces
              (when (>= (count space) (count (first transformations)))
                (my-mapcat (partial possible-inserts space) transformations))]
          (some (fn [remaining-space]
                  (fit shapes remaining-space (rest pieces))) remaining-spaces)))))

(let [shapes (into {}
                   (map (fn [[index shape]]
                          [index (possible-transformations shape)]))
                   (first (parse-input sample)))
      {:keys [size counts]} (-> sample parse-input second second)
      pieces (mapcat (fn [[x n]] (repeat n x)) counts)
      space (layout-initial-space size)]
  (fit shapes space pieces))

#_(let [parsed (parse-input sample)
      shapes (into {}
                   (map (fn [[index shape]]
                          [index (possible-transformations shape)]))
                   (first parsed))
      recipes (second parsed)]
  (->> recipes
       (filter (fn [{:keys [size counts]}]
                 (let [pieces (mapcat (fn [[x n]] (repeat n x)) counts)
                       space (layout-initial-space size)]
                   (fit shapes space pieces))))
       count))
