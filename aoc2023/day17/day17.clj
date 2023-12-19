(ns day17
  (:require [clojure.string :as str])
  (:import [java.util Comparator PriorityQueue]))

(set! *warn-on-reflection* true)

(def sample-input (slurp "day17/sample.txt"))
(def ^String puzzle-input (slurp "day17/input.txt"))

(defn parse-input [s]
  (->> s
       str/trim
       str/split-lines
       (map #(vec (map (comp abs (partial - (int \0)) int) %)))
       vec))

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(defn may-move? [m pos dir]
  (let [height (count m)
        width (count (first m))
        [y x] (dir pos)]
    (and (<= 0 y (dec height))
         (<= 0 x (dec width)))))

(def dirs {nil [south east]
           west [west north south]
           east [east north south]
           north [north east west]
           south [south east west]})

(defn safe+ [& args]
  (reduce (fn [acc n]
            (if (or (= Long/MAX_VALUE acc)
                    (= Long/MAX_VALUE n))
              (reduced Long/MAX_VALUE)
              (+ acc n))) args))

(defn direction [[y x] [y' x']]
  (if (= y y')
    (cond
      (< x x') east
      (> x x') west
      :else nil)
    (cond
      (< y y') south
      (> y y') north
      :else nil)))

(defn reconstruct-path [came-from current]
  (loop [total-path (list current)
         current current]
    (if-let [c' (came-from current)]
      (recur (cons c' total-path)
             c')
      total-path)))

(defn follow-path-back [prev u]
  (->> (reconstruct-path prev u)
       (reverse)
       (take 4)
       (remove nil?)
       (partition 2 1)
       (map #(direction (first %)
                        (second %)))))

(defn evaluate-path [m p]
  (->> p
       butlast
       (map #(get-in m %))
       (reduce +)))

(defrecord QueueElement [heat pos visited speed dir])

(defn djikstra [m]
  (let [source [0 0]
        max-y (dec (count m))
        max-x (dec (count (first m)))
        target [max-y max-x]
        Q (PriorityQueue. (reify
                            Comparator
                            (compare ^int [this o1 o2]
                              (let [[y1 x1] (:pos o1)
                                    [y2 x2] (:pos o2)]
                                (- (+ (:heat o1)
                                      (+ (- max-y y1)
                                         (- max-x x1)))
                                   (+ (:heat o2)
                                      (+ (- max-y y2)
                                         (- max-x x2))))))))]
    (.add Q (->QueueElement 0 source #{source} 0 nil))
    (loop []
      (let [u (.remove Q)]
        (if (= (:pos u) target)
          u
          (let [heat-loss (:heat u)
                pos (:pos u)
                speed-limit (>= (:speed u) 3)
                dir (:dir u)
                neighbours (into []
                                 (comp
                                  (filter #(if speed-limit
                                             (not= % dir)
                                             %))
                                  (filter #(may-move? m pos %))
                                  (map #(% pos))
                                  (remove (:visited u)))
                                 (dirs dir))]
            (doseq [v neighbours]
              (let [dir (direction pos v)]
                (.add Q (->QueueElement (+ heat-loss (get-in m v))
                                        v
                                        (conj (:visited u) v)
                                        (if (not= dir (:dir u))
                                          1
                                          (inc (:speed u)))
                                        dir))))
            (recur)))))))

;; I think this needs a more efficient priority queue
(def result (djikstra (parse-input sample-input)))

#_(result [12 12])

#_(reconstruct-path result [12 12])

#_(search-path  (parse-input puzzle-input))

#_(let [m (parse-input sample-input)]
  (reduce (fn [acc pos]
            (+ acc (get-in m pos)))
          0
          (reconstruct-path result [12 12])))

(defn visualize [m steps dirs]
  (let [height (count m)
        width (count (first m))]
    (str/join
     \newline
     (for [y (range height)]
       (str/join
        (for [x (range width)]
          (if (contains? steps [y x])
            (let [d (last (dirs [y x]))]
              (cond
                (= west d) \<
                (= east d) \>
                (= north d) \^
                (= south d) \v
                :else \#))
           (get-in m [y x]))))))))

#_(let [m (parse-input sample-input)
      p (set (second (result [12 12])))]
  (println "---------")
  (println (visualize m p {})))
