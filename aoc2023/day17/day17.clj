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
           west [#_west north south]
           east [#_east north south]
           north [#_north east west]
           south [#_south east west]})

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

(defn evaluate-path [m p]
  (->> p
       butlast
       (map #(get-in m %))
       (reduce +)))

(defrecord QueueElement [heat pos visited speed dir])

(defn distance [[y1 x1] [y2 x2]]
  (+ (abs (- y2 y1)) (abs (- x2 x1))))

(defn neighbours-in-dir [m current d']
  (:acc
   (reduce (fn [{:keys [acc new-heat
                        pos] :as all} n]
             (let [speed' (+ n
                             (if (= d' (:dir current))
                               (:speed current)
                               0))]
               (if (or (> speed' 3)
                       (not (may-move? m pos d')))
                 all
                 (let [pos' (d' pos)
                       heat' (+ new-heat (get-in m pos'))]
                   {:acc (cons [pos' heat' speed' d'] acc)
                    :new-heat heat'
                    :pos pos'}))))
           {:acc '()
            :pos (:pos current)
            :new-heat 0}
           (range 1 4))))

(defn astar [m start goal h]
  (let [height (count m)
        width (count (first m))
        visited (atom #{})
        f-score (atom {start (h start)})
        g-score (atom {start 0})
        ^PriorityQueue open-set (PriorityQueue.
                                 (reify Comparator
                                   (compare ^int [_this o1 o2]
                                     (- (+ (:heat o1)
                                           (distance goal (:pos o1)))

                                        #_(@f-score o1 Long/MAX_VALUE)
                                        (+ (:heat o2)
                                           (distance goal (:pos o2)))
                                        #_(@f-score o2 Long/MAX_VALUE)))))]
    (.add open-set (->QueueElement 0 [0 0] '([0 0]) 0 nil))
    (loop []
      (if (.isEmpty open-set) @g-score
          (let [current (.remove open-set)]
            (if (= (:pos current) goal)
              current
              (do
                (when-not (contains? @visited [(:pos current)
                                               (:dir current)])
                  (swap! visited conj [(:pos current) (:dir current)])
                  (doseq [[n-pos n-heat n-speed n-dir]
                          (->> (dirs (:dir current))
                               (mapcat (partial neighbours-in-dir m current))
                               reverse)]
                    (let [#_#_tentative-g-score (safe+ (:heat current) n-heat)]
                      (when true #_(< tentative-g-score (@g-score n-pos Long/MAX_VALUE))
                            #_(swap! g-score assoc n-pos tentative-g-score)
                            #_(swap! f-score assoc n-pos (safe+ tentative-g-score (h n-pos)))
                        (.add open-set (->QueueElement
                                        (+ (:heat current) n-heat)
                                        n-pos
                                        (cons n-pos (:visited current))
                                        n-speed
                                        n-dir))))))
                (recur))))))))


((astar (parse-input sample-input) [0 0] [12 12] (fn [a] (distance [12 12] a))) [12 12])

(def res (astar (parse-input sample-input) [0 0] [12 12] (fn [a] (distance [12 12] a))))

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


(let [m (parse-input sample-input)
      p (set (:visited res))]
  (println "---------")
  (println (visualize m p {})))

(let [m (parse-input (slurp "day17/input.txt"))
      max-y (dec (count m))
      max-x (dec (count (first m)))]
  (:heat (astar (parse-input (slurp "day17/input.txt")) [0 0] [max-y max-x] (fn [a] (distance [12 12] a)))))
