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

(defn safe+ [& args]
  (reduce (fn [acc n]
            (if (or (= Long/MAX_VALUE acc)
                    (= Long/MAX_VALUE n))
              (reduced Long/MAX_VALUE)
              (+ acc n))) args))

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
             f-score {start (h start)}
             v-score {start [nil 0]}]
        (let [[current speed dir :as all-current]
              (if (empty? open-set) nil
                  (reduce (fn find-current [acc n]
                            (min-key
                             (fn keyfn [n] (f-score (first n) Long/MAX_VALUE)) acc n))
                          open-set))]
          (if (or (empty? open-set)
                  (= current goal))
            [current came-from g-score f-score]
            (let [possible-dirs (if (nil? dir) [east south]
                                    (filter (fn allowed-move? [dir']
                                              (may-move? m current dir'))
                                            (dirs dir)))
                  new-state
                  (reduce (fn [{:keys [f-score
                                       g-score
                                       v-score
                                       open-set
                                       came-from] :as all} d]
                            (let [neighbor (d current)
                                  new-speed (inc (if (= dir d) (or speed 1) 0))
                                  tentative-score (safe+ (if (< 3 new-speed)
                                                           Long/MAX_VALUE
                                                           0)
                                                         (g-score current Long/MAX_VALUE)
                                                         (get-in m neighbor)
                                                         (let [[dn vn] (v-score neighbor)]
                                                           (if (not= dn d)
                                                             ;; Changing
                                                             ;; direction
                                                             ;; Doesn't seem to
                                                             ;; to to the other
                                                             ;; way
                                                             0
                                                             (if (< 3 (+ vn new-speed))
                                                               Long/MAX_VALUE
                                                               0))))]
                              (if (<= (g-score neighbor Long/MAX_VALUE) tentative-score)
                                all
                                {:came-from (assoc came-from neighbor current)
                                 :g-score (assoc g-score neighbor tentative-score)
                                 :f-score (assoc f-score neighbor (+ tentative-score (h neighbor)))
                                 :v-score (assoc v-score neighbor [d new-speed])
                                 :open-set (if (not-any? #(= % [neighbor new-speed d]) open-set)
                                             (cons [neighbor new-speed d]
                                                   open-set)
                                             open-set)})))
                          {:open-set (remove #(= all-current %) open-set)
                           :f-score f-score
                           :g-score g-score
                           :came-from came-from
                           :v-score v-score}
                          possible-dirs)]
              (recur
               ;; open-set
               (:open-set new-state)
               ;; came-from
               (:came-from new-state)
               ;; g-score
               (:g-score new-state)
               ;; f-score
               (:f-score new-state)
               (:v-score new-state)))))))))

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

(search-path  (parse-input puzzle-input))

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
