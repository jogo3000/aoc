(def sample-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
")

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn count-points [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\s*[:\|]\s*"))
       (map (fn [[card win-numbers numbers]]
              (let [win-numbers (set (str/split win-numbers #"\s+"))
                    card-numbers (set (str/split numbers #"\s+"))
                    matches
                    (count (set/intersection win-numbers card-numbers))]
                (if (>= matches 1)
                  (java.lang.Math/pow 2 (dec matches))
                  0))))
       (reduce +)))

(java.lang.Math/pow 2 0)

(count-points sample-input) ; 13.0


(count-points (slurp "/home/uusitalo/git/aoc/aoc2023/day4/input 2.txt")) ; 21158.0

;; part 2

(defn count-card-values [s]
  (->> s
       str/split-lines
       (map #(str/split % #"\s*[:\|]\s*"))
       (map (fn [[card win-numbers numbers]]
              (let [win-numbers (set (str/split win-numbers #"\s+"))
                    card-numbers (set (str/split numbers #"\s+"))
                    card-no (parse-long (str/trim (subs card (count "Card "))))
                    matches
                    (count (set/intersection win-numbers card-numbers))]
                [card-no matches])))
       (into {})))

(defn count-cards [m [card-no value]]
  (cond
    (nil? card-no) 0
    (zero? value) 1
    :else
    (+ 1
       (->> (range (inc card-no)
                   (inc (+ card-no value)))
            (map #(count-cards m [% (m %)]))
            (reduce +)))))

(def sample-card-values (count-card-values sample-input))

(count-cards sample-card-values [3 (sample-card-values 3)])

(reduce + (for [card (drop 2 (sort sample-card-values))]
           (count-cards sample-card-values card)))

(sort sample-card-values)

(reduce +
        (for [card sample-card-values]
          (count-cards sample-card-values card)))

(let [card-values (count-card-values (slurp "/home/uusitalo/git/aoc/aoc2023/day4/input 2.txt"))]
  (reduce +
        (for [card card-values]
          (count-cards card-values card))))

;; 6050769
