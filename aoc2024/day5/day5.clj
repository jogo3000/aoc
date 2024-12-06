(ns day5
  (:require [clojure.string :as str]))

(def sample-input (slurp "day5/sample-input"))

(def puzzle-input (slurp "day5/input"))

(defn parse-input [s]
  (let [[rules-str updates-str] (str/split s #"\n\n")]
    {:rules (->> (str/split-lines rules-str)
                 (map #(str/split % #"\|"))
                 (map #(map parse-long %)))
     :updates (->> (str/split-lines updates-str)
                   (map #(str/split % #","))
                   (map #(map parse-long %)))}))

(defn verify-update-spec [rules update-spec]
  (let [spec-count (count update-spec)]
    (every? true?
            (for [i (range spec-count)
                  :let [page-a (nth update-spec i)]]
              (->> (range (inc i) spec-count)
                   (every? (fn [j]
                             (let [page-b (nth update-spec j)]
                               (not-any? #(= [page-b page-a] %) rules)))))))))

(defn solve-1 [input]
  (let [{:keys [rules updates]} (parse-input input)]
    (->> (filter (partial verify-update-spec rules) updates)
         (map (fn [pages]
                (-> (split-at (/(count pages) 2) pages)
                    first last)))
         (apply +))))

(solve-1 puzzle-input); 4774
