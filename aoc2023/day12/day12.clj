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
       (keep-indexed (fn [n c]
                       (when (= damaged c)
                         n)))
       (reduce (fn [acc n]
                 (bit-set acc n)) 0)))

(defn to-row [n]
  (loop [n n
         s ""]
    (if (zero? n)
      s
      (recur (bit-shift-right (bit-clear n 0) 1)
             (str s (if (bit-test n 0)
                      damaged
                      operational))))))

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

(to-mask "??..")

(to-mask "??.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#???")

(defn find-max-bit [^BigInteger n]
  (loop [c 0
         n n]
    (if (zero? n)
      c
      (recur (inc c) (.shiftRight n 1)))))


(defn find-possible-placements [row group]
  (let [rowcount (count row)]
    (->> rowcount
         range
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
                   (let [s (subvec row 0 pos)]
                     (loop [p 0
                            bs 0]
                       (cond
                         (= bs group) true
                         (= p pos) false
                         :else
                         (recur (inc p)
                                (if (= \# (get row p))
                                  (inc bs)
                                  0))))))))))

(set! *warn-on-reflection* true)

(let [row "???.###"
      cs '(1 1 3)]
  (->> (find-possible-placements (vec row) 1)))
; (0 1 2)

(defn count-arrangements2 [row cs]
  (let [svec (vec row)]
    (loop [arrs 0
           queue (list (list svec cs))]
      (if (empty? queue) arrs
          (let [head (first queue)
                row (first head)
                rowcount (count row)
                groups (second head)
                c (first groups)
                placements (find-possible-placements row c)]
            (recur (+ arrs (if (and (empty? groups)
                                    (every? #{unknown operational} row)) 1 0))
                   (into (rest queue)
                         (comp
                          (map #(list (subvec row (min rowcount
                                                       (+ (inc %) c)))
                                      (rest (second head))))
                          (filter #(not (and (empty? (first %))
                                             (seq (second %))))))
                         placements)))))))

(count-arrangements2 "?###????????" (list 3,2,1))

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
