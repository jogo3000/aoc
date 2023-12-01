(require '[clojure.string :as str])

(defn process-line [s]
  (->> s
       (filter #(Character/isDigit %))
       ((fn [ds]
          (parse-long (str (first ds) (last ds)))))))

(->>
 (slurp "/home/uusitalo/git/aoc/aoc2023/day1/input.txt")
 (str/split-lines)
 (map process-line)
 (reduce +))
