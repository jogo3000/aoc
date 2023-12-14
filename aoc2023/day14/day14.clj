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

(-> (slurp "/home/uusitalo/git/aoc/aoc2023/day14/input.txt")
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

(defn find-loads [input]
  (loop [loads '()
         m (parse-input input)]
    (let [m' (run-cycle m)
          cycle-lengths
          (seq
           (for [p (range 2 (count loads))
                 :let [parts (partition p loads)]
                 :when (and (>= (count parts) 2)
                            (= 1 (count (set parts))))]
             p))]
      (if (or ;; (= (- 1000 4) (count loads))
              ;; Tähän oikea loppuehto
              cycle-lengths)
        [cycle-lengths loads]
        (recur (cons (evaluate-load m) loads) m')))))

(defn find-from-cycle [[[cycle-len & _] sequence] rounds]
  (let [len (count sequence)
        noncyclical (mod len cycle-len)
        zycle (take cycle-len sequence)]
    (nth zycle
         (mod (- cycle-len
                 (- (mod rounds cycle-len) noncyclical))
              cycle-len))))


(find-from-cycle (find-loads sample1) (inc 1000000000))

(find-from-cycle (find-loads (slurp "/home/uusitalo/git/aoc/aoc2023/day14/input.txt"))
                  (inc 1000000000))

;; 104671
