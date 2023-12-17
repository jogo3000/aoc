(require '[clojure.string :as str])


(defn process-line [s]
  (->> s
       (filter #(Character/isDigit %))
       ((fn [ds]
          (parse-long (str (first ds) (last ds)))))))

(->>
 (slurp "day1/input.txt")
 (str/split-lines)
 (map process-line)
 (reduce +))
;; Part 2

(def digits
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def sample2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def regex #"(one)|(1)|(two)|(2)|(three)|(3)|(four)|(4)|(five)|(5)|(six)|(6)|(seven)|(7)|(eight)|(8)|(nine)|(9)")

(re-find regex "t")

(defn my-findlast [st]
  (loop [s ""
         chrs (reverse st)]
    (if (or (empty? chrs) (re-find regex s))
      (first (re-find regex s))
      (recur (str (first chrs) s) (rest chrs)))))


(my-findlast "abcone2threexyz")

(defn find-first [st]
  (first (re-find regex st)))

(defn process-line2 [s]
  (let [ds
        (map #(digits % %)
             [(find-first s) (my-findlast s)])]
    (parse-long (str (first ds) (last ds)))))

(process-line2 "eighttwothree")

(->>
 sample2
 (str/split-lines)
 (map process-line2)
 )

(process-line2 "twonetwone")

(->>
 (slurp "day1/input.txt")
 (str/split-lines)
 (map process-line2)
 (reduce +))
