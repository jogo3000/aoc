(require '[clojure.string :as str])

(def sample-input
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn find-width [s]
  (-> s
      (str/split-lines)
      (first)
      (count)))

(defn find-height [s]
  (-> s str/split-lines count))

(defn ->map [s]
  {:height (find-height s)
   :width (find-width s)
   :game-map (str/split-lines s)})

(defn find-parts [input]
  (let [{:keys [height width game-map]} (->map input)]
    (println "----")
    (->>
     game-map
     (map-indexed
      (fn [n row]
        (let [m (-> (java.util.regex.Pattern/compile "\\d+") (.matcher row))]
          (loop [found []
                 pos 0]
            (if (or (>= pos width)
                    (not (.find m pos)))
              found
              (let [start (.start m)
                    end (.end m)
                    x-min (max 0 (dec start))
                    x-max (min width (inc end))
                    x-range (range x-min x-max)
                    y-min (max 0 (dec n))
                    y-max (min height (+ n 2))
                    y-range (range y-min y-max)
                    chars (into []
                                (for [y y-range
                                      x x-range]
                                  (get-in game-map [y x])))
                    part-number?
                    (some false?
                          (for [c chars]
                            (boolean (or (Character/isDigit c)
                                         (= \. c)))))]
                (println part-number? (subs row start end) n start)
                (println (str/join "\n" (map #(str/join %) (partition (- x-max x-min) chars))))
                (recur (if part-number?
                         (conj found (parse-long (subs row start end)))
                         found)
                       (inc end))))))))
     flatten)))


(->> sample-input
     find-parts
     (reduce +))

;; 4361 , correct

(->> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day3/input")
     find-parts
     #_(into #{})
     (reduce +))

;; part 2
(defn find-gears [input]
  (let [{:keys [height width game-map]} (->map input)]
    (->> game-map
         (map-indexed
          (fn [row-n row]
            (let [m (-> (java.util.regex.Pattern/compile "\\*") (.matcher row))]
              (loop [found []
                     pos 0]
                (if (or (>= pos width)
                        (not (.find m pos)))
                  found
                  (let [start (.start m)
                        end (.end m)
                        y-min (max 0 (dec row-n))
                        y-max (min height (+ row-n 2))
                        y-range (range y-min y-max)]
                    (recur
                     (conj found
                           (let [numbers
                                 (for [y y-range]
                                   (let [search-row (get game-map y)
                                         m (-> (java.util.regex.Pattern/compile "\\d+") (.matcher search-row))]
                                     (loop [found-parts []
                                            pos 0]
                                       (if (or (>= pos width)
                                               (not (.find m pos)))
                                         found-parts
                                         (recur
                                          (if (<= (dec (.start m)) start end (inc (.end m)))
                                            (conj found-parts (parse-long (subs search-row (.start m) (.end m))))
                                            found-parts) (inc (.end m)))))))
                                 found-parts (flatten numbers)]
                             (println found-parts)
                             (if (= 2 (count found-parts))
                               (apply * found-parts) 0))) (inc end))))))))
         flatten)))


(->> (find-gears sample-input)
     (reduce +))

(->> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day3/input")
     (find-gears)
     (reduce +))
