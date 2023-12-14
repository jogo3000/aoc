(ns day14
  (:require [clojure.string :as str]))

;;; I hate lava

;; Part 1
(def sample1
  "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(def rock \O)
(def solid \#)
(def free \.)

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv vec)))

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(defn rock? [m pos]
  (= (get-in m pos) rock))

(defn may-move? [m pos dir]
  (let [[y' x' :as pos'] (dir pos)]
    (and (not (or (neg? y') (neg? x'))) ; May not move out of the map
         (= (get-in m pos') free))))

(defn move [m pos dir]
  (let [pos' (dir pos)]
    (-> m
        (assoc-in pos free)
        (assoc-in pos' rock))))

(defn step [m dir]
  (let [height (count m)
        width (count (first m))]
    (reduce (fn [m pos]
              (cond
                (not (rock? m pos)) m
                (may-move? m pos dir) (move m pos dir)
                :else m))
            m
            (for [y (range height)
                  x (range width)]
              [y x]))))

(defn visualize [m]
  (->> m
       (map str/join)
       (str/join \newline)
       println))

(defn roll-until-stop [m dir]
  (loop [m m]
    (let [m' (step m dir)]
      (if (= m m')
        m
        (recur m')))))

(defn evaluate-load [m]
  (let [height (count m)]
    (->> m
         (map-indexed
          (fn [i row]
            (reduce (fn [acc c]
                      (if (= rock c)
                        (+ acc (- height i))
                        acc)) 0 row)))
         (reduce +))))

(evaluate-load (roll-until-stop (parse-input sample1) north))

(-> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day14/input.txt")
    str/trim
    parse-input
    (roll-until-stop north)
    evaluate-load)

;; 108889


;; Part deux

(defn run-cycle [m]
  (-> m
      (roll-until-stop north)
      (roll-until-stop west)
      (roll-until-stop south)
      (roll-until-stop east)))

(-> (parse-input sample1)
    run-cycle
    visualize) ;; looks ok

(set! *warn-on-reflection* true)

(->> sample1
     parse-input
     (iterate run-cycle)
     (take 50)
     (map evaluate-load))

(def found-loads
  (loop [loads '()
         m (parse-input sample1)]
    (let [m' (run-cycle m)
          cycle-lengths
          (seq
           (for [p (range 2 (count loads))
                 :let [parts (partition p loads)]
                 :when (and (>= (count parts) 2)
                            (= 1 (count (set parts))))]
             p))]
      (if (or (= (- 1000 4) (count loads))
              ;; Tähän oikea loppuehto
              cycle-lengths)
        [cycle-lengths loads]
        (recur (cons (evaluate-load m) loads) m')))))

(let [[ls sequence] found-loads
      cycle-len (first ls)
      len (count sequence)
      cycles (quot len cycle-len)
      noncyclical (mod len cycle-len)]
  {:cycle-len cycle-len
   :len len
   :cycles cycles
   :noncyclical noncyclical
   :result (rem (- 21 noncyclical) cycles)})

{:cycle-len 7, :len 17, :cycles 2, :noncyclical 3, :result 1}



;[7 17 3 3 1]

; x x x [ ... ] y

(clojure.pprint/pprint (partition 7 (second found-loads)))

(count (second found-loads))

'(65 69 69
  68 63 65 64 65 69 69
  68 63 65 64 65 69 69
                       69 87 104)

#_(104 87 69
       69 69 65 64 65 63 68
       69 69 65 64 65 63 68
       69 69 65 64 65 63 68
       69 69 65 64 65 63 68
       69 69 65 64 65 63 68
       69 69 65 64 65 63 68)


#_(->> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day14/input.txt")
     parse-input
     (iterate run-cycle)
     (take 50)
     (map evaluate-load))

'(101316
  100329
  100364
  100494
  100619
  100689
  100790
  100915
  100981
  101085 101150 101218 101304 101387 101446 101511 101617 101692 101779 101847 101917 101955 102041 102074 102127 102179 102228 102280 102376 102457 102552 102646 102751 102841 102951 103036 103151 103255 103392 103498 103606 103702 103819 103922 104046 104142 104233 104311 104416 104503)
