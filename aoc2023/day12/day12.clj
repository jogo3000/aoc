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
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(defn to-row [^BigInteger n]
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

(defn to-mask [row]
  (->> row
       reverse
       (keep-indexed (fn [n c]
                       (when (#{damaged unknown} c) n)))
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

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

(defrecord QueueTask [c pos groups])

(defn count-arrangements [row cs]
  (let [svec (vec row)
        rowcount (count row)
        ^BigInteger mask (to-mask row)]
    (loop [arrs 0
           all-arrs '()
           queue (list (->QueueTask BigInteger/ZERO 0 cs))]
      (if (empty? queue) all-arrs
          (let [head (first queue)
                ^BigInteger c (:c head)
                pos (:pos head)
                groups (:groups head)
                group (first groups)
                ^BigInteger spring-mask (spring-masks group)
                placements (find-possible-placements svec pos group)
                complete? (and (empty? groups)
                               (or (>= pos rowcount)
                                   (every? #{unknown operational} (subvec svec pos))))]
            #_(println (to-row c) placements)
            (recur (+ arrs (if complete? 1 0))
                   (if complete? (cons c all-arrs) all-arrs)
                   (into (rest queue)
                         (comp
                          (keep (fn [pp]
                                  (let [cand (.or c (.shiftLeft spring-mask (- rowcount pp (inc group))))]
                                    #_(println (to-row cand))
                                    (when (.equals cand (.and mask cand))
                                      (->QueueTask cand
                                                   (+ pp group 1)
                                                   (rest groups)))))))
                         placements)))))))

(defn count-total-arrangements [input]
  (->> input
       str/trim
       str/split-lines
       (map parse-row)
       (map (fn [[a b]]
              (count-arrangements a b)))
       (reduce (fn [acc arrs]
                 (+ acc (count arrs))) 0)))

(count-total-arrangements sample-2)

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
     count-total-arrangements)

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
           str/split-lines
           ((fn [s] (subvec s 40 41)))
           (map parse-row)
           (map (fn [[a b]]
                  (println a b)
                  (doseq [ar (count-arrangements a b)]
                    (let [s (to-row ar)]
                      (println (str (str/join (take (- (count a) (count s)) (repeat \.))) s)))))))

(def *arrs-1
  (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
       str/split-lines
       (map parse-row)
       (map-indexed (fn [i [a b]]
                      [i (count-arrangements a b)]))))

(->> "??.??.?????###? 1,2,6"
     parse-row
     (apply count-arrangements))

(->> "?#???#?#?????? 1,3,3"
     parse-row
     (apply count-arrangements))

; (6000 5916 5902 5895 4572 4558 4551 4215)

; (1046056 1044586 1044585 261226 261225 130666 130665)

(println "??.??.?????###? 1,2,6")
(->> "??.??.?????###? 1,2,6"
     day12-old/count-arrangements)
; 5

(println "?????#???#?.??#?#??? 8,2,1,1")
(->> "?????#???#?.??#?#??? 8,2,1,1"
     day12-old/count-arrangements)

(clojure.set/difference
 (set (map #(update % 1 count) *arrs-1))
 (set *arrs-old))

(def *arrs-old
  (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
       day12-old/total-arrangements-per-line))

;; 7747 giving too high answer, but why?

;; 7173 -- is the correct answer

;; part deux

(defn expand-row [row]
  (let [[springs counts] (str/split row #"\s+")]
    (str (str/join \? (repeat 5 springs))
         " "
         (str/join \, (repeat 5 counts)))))



(parse-row (first (str/split-lines sample-2)))



(count (find-unknowns "??.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#???"))

;; 74, amount of permutations 2^74, that's not going to fly

(defn find-max-bit [^BigInteger n]
  (loop [c 0
         n n]
    (if (zero? n)
      c
      (recur (inc c) (.shiftRight n 1)))))





(set! *warn-on-reflection* true)

(let [row "???.###"
      cs '(1 1 3)]
  (->> (find-possible-placements (vec row) 0 1)))
; (0 1 2)





(->> sample-2
     str/split-lines
     (map expand-row)
     (map parse-row)
     (map (fn [[r c]] (count-arrangements2 r c))))


(->> (expand-row ".???.??.?? 1,1,1")
     parse-row
     (apply count-arrangements2))

(def *arrs
  (doall
   (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
        str/split-lines
        (map expand-row)
        (map parse-row)
        (map-indexed (fn [i [r c]]
               (println i)
               (count-arrangements2 r c))))))
