(ns day12
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)


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

(defn to-binary
  "Note that the binary pattern will look like it is reversed!"
  [row]
  (->> row
       (keep-indexed (fn [n c]
                       (when (= damaged c)
                         n)))
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(.toString (to-binary "?.##..###.") 2) ;; "111110011111100000"

(defn to-row [^BigInteger n]
  (loop [n n
         s ""]
    (if (zero? n)
      s
      (recur (.shiftRight (.clearBit n 0) 1)
             (str s (if (.testBit n 0)
                    damaged
                    operational))))))

(defn parse-row [row]
  (let [[springs & counts] (str/split row #"[\s,]+")]
    [springs
     (map parse-long counts)]))

(defn to-mask
  "Binary pattern will look reversed!"
  [row]
  (->> row
       reverse
       (keep-indexed (fn [n c]
                       (when (#{damaged unknown} c) n)))
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(.toString (to-mask "?.??.???") 2) ; "11101101"


(defn to-dm-mask [row]
  (->> row
       reverse
       (keep-indexed (fn [n c]
                       (when (= damaged c) n)))
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(.toString (to-dm-mask "????.######..#####.") 2) ; "11111100111110"

(defn to-op-mask [row]
  (->> row
       reverse
       (keep-indexed (fn [n c]
                       (when (= operational c) n)))
       (reduce (fn [^BigInteger acc n]
                 (.setBit acc n)) BigInteger/ZERO)))

(.toString (to-dm-mask "?.##..??#") 2) "1100001"
(.toString (to-op-mask "?.##...?#") 2) "10011000"

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


(def spring-masks (into {}
                        (map (fn [[i v]]
                               [i (BigInteger/valueOf v)]))
                        {1 2r1
                         2 2r11
                         3 2r111
                         4 2r1111
                         5 2r11111
                         6 2r111111
                         7 2r1111111
                         8 2r11111111
                         9 2r111111111
                         10 2r1111111111
                         11 2r11111111111
                         12 2r111111111111
                         13 2r1111111111111
                         14 2r11111111111111
                         15 2r111111111111111
                         16 2r1111111111111111
                         17 2r11111111111111111
                         18 2r111111111111111111
                         19 2r1111111111111111111
                         20 2r11111111111111111111}))

(defn to-cand-mask [mask group offset]
  (.and (.shiftRight mask offset)
        (-> (.pow (BigInteger/valueOf 2) (inc group))
            (.subtract BigInteger/ONE))))

(defn count-arrangements [[row cs]]
  (let [^BigInteger mask (to-mask row)
        ^BigInteger dm-mask (to-dm-mask row)
        ^BigInteger op-mask (to-op-mask row)
        row (vec (->> row reverse)) ;; Drop leading operational, they make no difference in the calculation
        ;; Need to reverse the groups because be bit representation is mirrored
        cs (reverse cs)
        rowcount (count row)]
    (letfn [(count-arrangements* [mem-count start #_#_^BigInteger mask ^BigInteger dm-mask cs]
              (if (and (empty? cs)
                       (or (>= start rowcount)
                           (every? (fn [ch] (or (= \? ch)
                                                (= \. ch))) (subvec row start))))
                1
                (let [group (first cs)
                      placements (find-possible-placements row start group)
                      spring-mask (spring-masks group)]
                  (reduce +
                          (keep (fn [p]
                                  (let [cand (.shiftLeft spring-mask (- p start))]
                                    (println "----- " p)
                                    (println (.toString cand 2))
                                    (println (.toString (.shiftRight mask start) 2))
                                    (println (.toString (to-cand-mask dm-mask group start) 2))
                                    (when (and
                                           ;; Candidate has no damaged springs in place of operational from mask
                                           (.equals cand (.and (.shiftRight mask start) cand))
                                           ;; Candidate has no operational springs in place of damaged from dm-mask
                                           ;; Call to cand-mask is probably wrong
                                           (let [cand-dm-mask (to-cand-mask dm-mask group start)]
                                             (.equals cand-dm-mask (.and cand-dm-mask cand))))
                                      (mem-count mem-count
                                                 (+ p group 1) ; start
                                                 (rest cs)))))
                                placements)))))]
      (let [mem-count (memoize count-arrangements*)]
        (mem-count mem-count 0 #_BigInteger/ZERO cs)))))

(count-arrangements (parse-row ".??..??...?##. 1,1,3"))
(.toString (to-dm-mask ".??..??...?##.") 2) ; "10011001110001"
#_(count-arrangements (parse-row ".??..??...?##. 1,1,3")) ; should be 4

(count-arrangements (parse-row "????.#...#... 4,1,1")) ;; Should be 1

(defn count-total-arrangements [input]
  (->> input
       str/split-lines
       (map parse-row)
       (map count-arrangements)
       (reduce +)))

(count-total-arrangements sample-2) ; Should be 21

(count-total-arrangements (slurp "/home/jogo3000/git/aoc2022/aoc2023/day12/input.txt"))
;; 7208 -- too high answer
;; 7173 -- is the correct answer

;; part deux

(defn expand-row [row]
  (let [[springs counts] (str/split row #"\s+")]
    (str (str/join \? (repeat 5 springs))
         " "
         (str/join \, (repeat 5 counts)))))

(count (find-unknowns "??.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#??????.??.???###????#???"))

;; 74, amount of permutations 2^74, that's not going to fly

(defn find-max-bit [^BigInteger n]
  (loop [c 0
         n n]
    (if (zero? n)
      c
      (recur (inc c) (.shiftRight n 1)))))

(->> sample-2
     str/split-lines
     (map expand-row)
     (map parse-row)
     (map count-arrangements2))


(def *arrs
    (doall
     (->> (slurp "/home/uusitalo/git/aoc/aoc2023/day12/input.txt")
          str/split-lines
          (map expand-row)
          (map parse-row)
          (map-indexed (fn [i arrs]
                         (println i)
                         (count-arrangements2 arrs))))))
