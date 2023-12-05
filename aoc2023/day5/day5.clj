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

;; part 2

(defn find-lowest2 [m]
  (let [seeds (->> (:seeds m)
                   (partition 2)
                   (mapcat (fn [[start l]]
                             (range start (+ start l)))))]

    (reduce (fn [acc n]
              (let [loc (find-location m n)]
                (if-not acc loc (min acc loc))))
            nil seeds)))

(def puzzle-seeds
  (-> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
      (parse-input)
      :seeds))

(reduce (fn [acc [_ l]]
          (+ acc l)) 0 (partition 2 puzzle-seeds))


;; 2 221 837 783
;; TOO MUCH

(+ 3127166940 109160474)

(map (fn [[s l]]
       [s (+ s l)]) (partition 2 puzzle-seeds))

;; [3127166940
;;  3236327414]
;; [3265086325
;;  3351535909]
;; [1581539098
;;  1786744824]
;; [3646327835
;;  3831071286]
;; [2671979893
;;  2689128044]
;; [305618297
;;  346020154]
;; [2462071712
;;  2665146912]
;; [358806266
;;  489953612]
;; [1802185716
;;  2340712460]
;; [635790399
;;  1341769649]

;; Looks like the ranges aren't overlapping, need to figure out a more efficient data structure probably

(:seeds sample-maps)

(find-lowest2 sample-maps) ;; 46, correct

(comment
  (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day5/input.txt")
       (parse-input)
       (find-lowest2)))
