(require '[clojure.string :as str])

(def sample "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def puzzle-input (slurp "/home/jogo3000/git/aoc2022/aoc2023/day2/input.txt"))

(defn parse-line [s]
  (let [[game & cubes] (str/split s #"[;:]")
        gameno (parse-long (str/join (drop (count "Game ") game)))
        cube-counts (for [st cubes]
                      (map (fn [s]
                             (let [[no color]
                                   (str/split (str/trim s) #" +")]
                               [(parse-long no) color]))
                           (-> st
                               (str/trim)
                               (str/split #","))))]
    [gameno cube-counts]))

(def max-counts {"red" 12 "green" 13 "blue" 14})

(defn count-possible [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (keep (fn [[id sets]]
               (when-not
                   (some true?
                         (for [s sets]
                           (some (fn [[n color]] (< (max-counts color) n)) s)))
                 id)))
       (reduce +)))

(count-possible puzzle-input)

;; part 2

(defn find-power-of-minimum [game]
  (let [[_ sets] game]
    (->> sets
         (reduce (fn [acc cubes]
                   (->> cubes
                        (reduce (fn [acc [n color]]
                                  (assoc acc color (max n (acc color 0)))) acc)))
                 {})
         vals
         (reduce *))))

(->> puzzle-input
     (str/split-lines)
     (map parse-line)
     (map find-power-of-minimum)
     (reduce +))

;; 63981
