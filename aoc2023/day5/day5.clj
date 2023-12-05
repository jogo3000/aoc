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
                                             {:source src
                                              :destination dest
                                              :length c})))])))
       (into {})))



(def sample-maps
  (parse-input sample-input))

(defn navigate [ranges n]
  (if-let [dest (some (fn [{:keys [source destination] l :length}]
                        (when (<= source n (+ source l))
                          (+ destination (- n source))))
                      ranges)]
    dest
    n))

(defn find-location [m n]
  (->> n
       (navigate (:seed-to-soil m))
       (navigate (:soil-to-fertilizer m))
       (navigate (:fertilizer-to-water m))
       (navigate (:water-to-light m))
       (navigate (:light-to-temperature m))
       (navigate (:temperature-to-humidity m))
       (navigate (:humidity-to-location m))))

(defn find-lowest [m]
  (->> (:seeds m)
       (map (partial find-location m))
       sort
       first))

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
     (parse-input)
     (find-lowest))

;; 218513636
