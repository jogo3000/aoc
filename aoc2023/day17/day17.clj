(ns day17
  (:require [clojure.string :as str]))

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

(defn djikstra [m]
  (let [source [0 0]
        height (count m)
        width (count (first m))
        target [(dec height) (dec width)]]
    (loop [Q #{[0 [source]]}]
      (println (count Q))
      (let [u (reduce (fn priority-pick [acc u]
                        (if (< (first acc) (first u))
                          acc u)) Q)]
        (if (or (empty? Q)
                (= (first (second u)) target))
          u
          (let [Q (disj Q u)

                heat-loss (first u)
                pos (second u)
                path-here (when (<= 3 (count pos))
                            (->> (take 4 pos)
                                 (partition 2 1)
                                 (map #(direction (second %) (first %)))))
                speed-limit (and (= (count path-here) 3)
                                 (= (count (set path-here)) 1))
                #_#__ (when speed-limit (println "limiting speed" path-here (set path-here)))
                dir (when path-here
                      (first path-here))
                possible-directions (->> (dirs dir)
                                         (filter #(if speed-limit
                                                    (not= % dir)
                                                    %))
                                         (filter #(may-move? m (first pos) %)))
                neighbours (map #(% (first pos)) possible-directions)
                Q (into Q
                        (map (fn [v] [(+ heat-loss
                                         (get-in m v))
                                      (cons v pos)]))
                        neighbours)]
            (recur Q)))))))

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
