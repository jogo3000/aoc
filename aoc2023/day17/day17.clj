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

(def dirs {west [west north south]
           east [east north south]
           north [north east west]
           south [south east west]})

(def visited (atom #{}))

(defn search-path [m]
  (let [height (count m)
        width (count (first m))
        start [0 0]
        goal [(dec height) (dec width)]]
    (letfn [(h [pos]
              (+ (abs (- (first goal) (first pos)))
                 (abs (- (second goal) (second pos)))))]
      (loop [open-set (list [start 0 nil])
             came-from {}
             g-score {start 0}
             f-score {start (h start)}]
        (let [[current speed dir :as all-current]
              (if (empty? open-set) nil
                  (reduce (fn find-current [acc n]
                            (min-key
                             (fn keyfn [n] (f-score (first n) Long/MAX_VALUE)) acc n))
                          open-set))]
          (swap! visited conj all-current)
          (if (or (empty? open-set)
                  (= current goal))
            [current came-from g-score f-score]
            (let [possible-dirs (if (nil? dir) [east south]
                                    (filter (fn allowed-move? [dir']
                                              (and
                                               (may-move? m current dir')
                                               (not (and (>= (or speed 0) 3) (= dir dir')))))
                                            (dirs dir)))
                  new-state
                  (reduce (fn [{:keys [f-score g-score open-set came-from] :as all} d]
                            (let [neighbor (d current)
                                  new-speed (inc (if (= dir d) (or speed 0) 0))
                                  tentative-score (+ (g-score current Long/MAX_VALUE)
                                                     (get-in m neighbor))]
                              (if (< (g-score neighbor Long/MAX_VALUE) tentative-score)
                                all
                                {:came-from (assoc came-from neighbor current)
                                 :g-score (assoc g-score neighbor tentative-score)
                                 :f-score (assoc f-score neighbor (+ tentative-score (h neighbor)))
                                 :open-set (if (not-any? #(= % [neighbor new-speed d]) open-set)
                                             (cons [neighbor
                                                    new-speed
                                                    d]
                                                   open-set)
                                             open-set)})))
                          {:open-set (remove #(= all-current %) open-set)
                           :f-score f-score
                           :g-score g-score
                           :came-from came-from}
                          possible-dirs)]
              (recur
               ;; open-set
               (:open-set new-state)
               ;; came-from
               (:came-from new-state)
               ;; g-score
               (:g-score new-state)
               ;; f-score
               (:f-score new-state)))))))))

(defn reconstruct-path [came-from current]
  (loop [total-path (list current)
         current current]
    (if-let [c' (came-from current)]
      (recur (cons c' total-path)
             c')
      total-path)))

(def result (search-path (parse-input sample-input)))
((nth result 2) (first result))
(reconstruct-path (second result) (first result))

(let [m (parse-input sample-input)]
  (reduce (fn [acc pos]
            (+ acc (get-in m pos)))
          0
          (reconstruct-path (second result) (first result))))

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
      p (set (reconstruct-path (second result) (first result)))]
  (println "---------")
  (println (visualize m p (second result))))
