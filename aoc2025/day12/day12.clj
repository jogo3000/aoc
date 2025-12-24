(ns day12
  (:require [clojure.string :as str]))

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

(let [shape (get-in (parse-input sample) [0 0])]
  (println (draw-shape shape))
  (println)
  (println (draw-shape (reflect shape)))
  (println)
  (println (draw-shape (rotate (rotate shape))))
  (println))
