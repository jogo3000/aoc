(ns day12
  (:require [clojure.string :as str]))

(def operational \.)
(def damaged \#)
(def unknown \?)

(def sample1 "#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1")

(def sample-2 "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn find-unknowns [row]
  (keep-indexed #(when (= unknown %2) %1) row))

(defn to-binary [row]
  (->> row
       reverse
       (keep-indexed (fn [n c]
                       (when (= damaged c)
                         n)))
       (reduce (fn [acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(defn to-row [n]
  (loop [n n
         s ""]
    (if (zero? n)
      s
      (recur (.shiftRight (.clearBit n 0) 1)
             (str (if (.testBit n 0)
                      damaged
                      operational) s)))))

(defn parse-row [row]
  (let [[springs & counts] (str/split row #"[\s,]+")]
    [springs
     (map parse-long counts)]))

(defn check-arrangement [row counts]
  (->> row
       (re-seq #"#+")
       (map count)
       (= counts)))

(defn row-permutations [row-binary permutations unknowns]
  (map to-row
       (for [p permutations]
         (loop [pn p
                u' unknowns
                n' row-binary]
           (if (zero? pn)
             n'
             (recur (bit-shift-right (bit-clear pn 0) 1)
                    (rest u')
                    (if (bit-test pn 0)
                      (bit-set n' (first u'))
                      (bit-clear n' (first u')))))))))

(defn count-arrangements [row]
  (let [[line counts] (parse-row row)
        unknowns (find-unknowns line)
        permutations
        (-> (java.lang.Math/pow 2 (count unknowns))
            range)]
    (reduce (fn [acc n]
              (if (check-arrangement n counts)
                (inc acc)
                acc))
            0
            (row-permutations (to-binary line)
                              permutations
                              unknowns))))

(defn count-total-arrangements [input]
  (->> input
       str/split-lines
       (map count-arrangements)
       (reduce +)))

(->> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day12/input.txt")
     count-total-arrangements)

;; 7173

;; part deux

(defn expand-row [row]
  (let [[springs counts] (str/split row #"\s+")]
    (str (str/join \? (repeat 5 springs))
         " "
         (str/join \, (repeat 5 counts)))))



(parse-row (first (str/split-lines sample-2)))

(defn to-mask [row]
  (->> row
       (keep-indexed (fn [n c]
                       (when (#{damaged unknown} c) n)))
       (reduce (fn [acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(count (find-unknowns "??.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#???"))

;; 74, amount of permutations 2^74, that's not going to fly

(defn find-max-bit [^BigInteger n]
  (loop [c 0
         n n]
    (if (zero? n)
      c
      (recur (inc c) (.shiftRight n 1)))))


(defn find-possible-placements
  "Narrow search for placements by disallowing some impossibilities"
  [row start group]
  (let [rowcount (count row)]
    (->> rowcount
         (range start)
         (keep (fn [start]
                 (if-not (or (zero? start) (let [^char c (get row (dec start))]
                                             (or (= \? c)
                                                 (= \. c))))
                   nil
                   (loop [pos start
                          c 0]
                     (cond
                       (>= pos rowcount)
                       (when (= c group) start)

                       (= c group)
                       (when (let [^char c (get row pos)]
                               (or (= \. c)
                                   (= \? c)))
                         start)

                       (let [^char c (get row pos)]
                         (or (= \? c)
                             (= \# c)))
                       (recur (inc pos)
                              (inc c))

                       :else
                       nil)))))
         ;; Remove placements where known broken ones are left over
         (remove (fn [pos]
                   (let [s (subvec row start pos)]
                     (loop [p 0
                            bs 0]
                       (cond
                         (= bs group) true
                         (= p pos) false
                         :else
                         (recur (inc p)
                                (if (= \# (get s p))
                                  (inc bs)
                                  0))))))))))


(set! *warn-on-reflection* true)

(let [row "???.###"
      cs '(1 1 3)]
  (->> (find-possible-placements (vec row) 0 1)))
; (0 1 2)

(defrecord QueueTask [c pos groups])

(def spring-masks (into {}
                        (map (fn [[i v]]
                               [i (BigInteger/valueOf v)]))
                        {1 2r10
                         2 2r110
                         3 2r1110
                         4 2r11110
                         5 2r111110
                         6 2r1111110
                         7 2r11111110
                         8 2r111111110
                         9 2r1111111110
                         10 2r11111111110
                         11 2r111111111110
                         12 2r1111111111110
                         13 2r11111111111110
                         14 2r111111111111110
                         15 2r1111111111111110
                         16 2r11111111111111110
                         17 2r111111111111111110
                         18 2r1111111111111111110
                         19 2r11111111111111111110
                         20 2r111111111111111111110}))

(defn count-arrangements2 [row cs]
  (let [svec (vec row)
        rowcount (count row)
        mask (to-mask row)]
    (loop [arrs 0
           queue (list (->QueueTask BigInteger/ZERO 0 cs))]
      (if (empty? queue) arrs
          (let [head (first queue)
                c (:c head)
                pos (:pos head)
                groups (:groups head)
                group (first groups)
                spring-mask (spring-masks group)
                placements (find-possible-placements svec pos group)]
            (println pos (to-row c))
            (recur (+ arrs (if (and (empty? groups)
                                    (every? #{unknown operational} (subvec svec pos))) 1 0))
                   (into (rest queue)
                         (comp
                          (keep (fn [pp]
                                  (let [cand (.or c (.shiftLeft spring-mask (- rowcount pp (inc group))))]
                                    (when (.equals cand (.and mask cand))
                                      (->QueueTask cand
                                                   (+ pos pp group 1) ;; <- this goes funky in second round
                                                   (rest groups)))))))
                         placements)))))))

(count-arrangements2 "?###????????" (list 3,2,1))

(to-mask "?###????????")

(to-row (to-binary "?###????????"))

(count "###...")
(to-row (.shiftLeft (spring-masks 3) (- 6 3 1 3)))


(count-arrangements2 "?###?????????###????????" (list 3,2,1 3,2,1))

(count-arrangements2 "?#?#?#?#?#?#?#?" (list 1 3,1,6))

(let [[a b] (parse-row (expand-row "?###???????? 3,2,1"))]
  (count-arrangements2 a b))

506250

(expand-row "??.??.???###????#??? 1,2,8,1,1")

(to-mask "??.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#???")
"1,2,8,1,1,1,2,8,1,1,1,2,8,1,1,1,2,8,1,1,1,2,8,1,1"


(->> sample-2
     str/split-lines
     (map expand-row)
     (map parse-row)
     (map (fn [[r c]] (count-arrangements2 r c))))


(def *arrs
  (doall
   (->> (slurp "/home/jogo3000/git/aoc2022/aoc2023/day12/input.txt")
        str/split-lines
        (map expand-row)
        (map parse-row)
        (map (fn [[r c]] (count-arrangements2 r c))))))
