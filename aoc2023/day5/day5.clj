(require '[clojure.string :as str])

(def sample-input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
")

(defrecord Range [source source-end destination length])

(defn parse-input [s]
  (->> (str/split s #"[\n\r]{2,}?")
       (map #(filter (fn [ss] (not= "map:" ss)) (str/split % #"\s+")))
       (map (fn [[title & nrs]]
              (if (= "seeds:" title)
                [:seeds (map parse-long nrs)]
                [(keyword title) (->> nrs
                                      (map parse-long)
                                      (partition 3)
                                      (map (fn [[dest src c]]
                                             (->Range src (+ src (dec c)) dest c)))
                                      (sort-by :source))])))
       (into {})))



(def sample-maps
  (parse-input sample-input))

(defn navigate [ranges n]
  (loop [[curr & others] ranges]
    (cond
      (not curr) n
      (> (:source curr)  n) n
      (and (<= (:source curr) n)
           (<= n (:source-end curr))) (+ (:destination curr) (- n (:source curr)))
      :else (recur others))))

(defn find-location [m]
  (let [{:keys [seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature
                temperature-to-humidity humidity-to-location]}
        m]
    (fn [n]
      (->> n
           (navigate seed-to-soil)
           (navigate soil-to-fertilizer)
           (navigate fertilizer-to-water)
           (navigate water-to-light)
           (navigate light-to-temperature)
           (navigate temperature-to-humidity)
           (navigate humidity-to-location)))))

(defn find-lowest [m]
  (->> (:seeds m)
       (map (find-location m))
       sort
       first))

(->> sample-input
     parse-input
     find-lowest) ;; 35, correct

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
     (parse-input)
     (find-lowest))

;; 218513636

;; part 2

(defn find-lowest2 [m]
  (let [finder (find-location m)
        seeds (->> (:seeds m)
                   (partition 2)
                   (mapcat (fn [[start l]]
                             (range start (+ start l)))))]

    (reduce (fn [acc n]
              (let [loc (finder n)]
                (if-not acc loc (min acc loc))))
            nil seeds)))

(def puzzle-seeds
  (-> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
      (parse-input)
      :seeds))

(->> puzzle-seeds
     (partition 2)
     (mapcat (fn [[start l]]
               (range start (+ start l)))))

;; 2 221 837 783 searh space
;; TOO MUCH for brute force


((find-location sample-maps) 82)
(navigate (:seed-to-soil sample-maps) 82)
(navigate (:soil-to-fertilizer sample-maps) 84)
(navigate (:fertilizer-to-water sample-maps) 84)
(navigate (:water-to-light sample-maps) 84)
(navigate (:light-to-temperature sample-maps) 77)


(find-lowest2 sample-maps) ;; 46, correct

(println (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
              (parse-input)
              (find-lowest2)))
