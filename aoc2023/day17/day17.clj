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

(defn search-path [m start]
  (let [height (count m)
        width (count (first m))
        goal [(dec height) (dec width)]]
    (letfn [(h [pos]
              (+ (- (first goal) (first pos))
                 (- (second goal) (second pos))))]
      (loop [open-set (list start)
             came-from {}               ; Only direction needed I think
             g-score {start 0}
             f-score {start (h start)}]
        (println (count open-set))
        (let [current (if (empty? open-set) nil
                          (reduce (fn find-current [acc n]
                                    (min-key
                                     (fn keyfn [n] (f-score n Long/MAX_VALUE)) acc n))
                                  open-set))]
          (if (or (empty? open-set)
                  (= current goal))
            [current came-from g-score f-score]
            (let [[speed dir] (came-from current)
                  possible-dirs (if (nil? dir) [south east]
                                    (filter (fn allowed-move? [dir']
                                              (and
                                               (may-move? m current dir')
                                               (not (and (>= (or speed 1) 3) (= dir dir')))))
                                            (dirs dir)))
                  neighbor->tentative-score
                  (for [d possible-dirs
                        :let [neighbor (d current)
                              tentative-score (+ (g-score current Long/MAX_VALUE)
                                                 (get-in m neighbor))]
                        :when (< tentative-score (g-score neighbor Long/MAX_VALUE))]
                    [neighbor tentative-score d])]
              (recur
               ;; open-set
               (reduce
                (fn [open-set [neighbor _ _]]
                  (if (not-any? #(= % neighbor) open-set)
                    (cons neighbor open-set)
                    open-set))
                (remove #(= current %) open-set) neighbor->tentative-score)

               ;; came-from
               (into came-from
                     (map (fn [[n _ d]] [n [(inc (or speed 1)) d]]))
                     neighbor->tentative-score)

               ;; g-score
               (into g-score
                     (map (juxt first second))
                     neighbor->tentative-score)

               ;; f-score
               (into f-score
                     (map (fn [[neighbor tentative-score _]]
                            [neighbor (+ tentative-score (h neighbor))]))
                     neighbor->tentative-score)))))))))


(search-path (parse-input sample-input) [0 0])
