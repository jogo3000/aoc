(ns day15
  (:require [clojure.string :as str]))

(defn HASH [s]
  (reduce (fn [acc c]
            (rem (* (+ acc (int c)) 17)
                 256))
          0
          s))

(HASH "HASH")

(def sample "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(->> (str/split sample #",")
     (map str/trim)
     (map HASH)
     (reduce +))

(->> (str/split (slurp "/home/uusitalo/git/aoc/aoc2023/day15/input.txt") #",")
     (map str/trim)
     (map HASH)
     (reduce +))

; 509152

;; Part deux

(defn parse-label [s]
  (if (str/ends-with? s "-") ;; Remove direction
    [:remove [(subs s 0 (dec (count s)))]]
    [:add (str/split s #"=")]))

(defn add-to-box [box label]
  (loop [pos 0]
    (cond (>= pos (count box))
          (conj box label)

          (= (first (get box pos))
             (first label))
          (assoc box pos label)

          :else (recur (inc pos)))))

(defn remove-from-box [box label]
  (loop [pos 0]
    (cond (>= pos (count box))
          box

          (= (first (get box pos))
             (first label))
          (let [[head tail] (split-at (inc pos) box)]
            (into (vec (rest head)) tail))

          :else (recur (inc pos)))))

(defn install-lenses [instructions]
  (->> (str/split instructions #",")
       (map str/trim)
       (map parse-label)
       (reduce (fn [acc [instr label]]
                 (let [hashcode (HASH (first label))]
                   (update acc hashcode
                           (fn [box]
                             (let [box (or box [])]
                               (if (= instr :add)
                                 (add-to-box box label)
                                 (remove-from-box box label)))))))
               {})))

(defn focusing-power [input]
  (->> (install-lenses input)
       (mapcat (fn [[i box]]
                 (let [labels (map first box)]
                   (assert (= (count labels) (count (set labels)))))
                 (map-indexed (fn [j [_ focal-length]]
                                (* (inc i) ;; one plus box number
                                   (inc j) ;; slot number of the lens
                                   (parse-long focal-length))) box)))
       (reduce +)))

(focusing-power sample)
(focusing-power (slurp "/home/uusitalo/git/aoc/aoc2023/day15/input.txt"))
;; 231852
