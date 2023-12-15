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
