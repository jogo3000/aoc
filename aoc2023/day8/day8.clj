(ns day8
  (:require [clojure.string :as str]))

(def sample-input-1 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
")

(defn parse-input [s]
  (let [[turns _ & nodes] (str/split-lines s)]
    [turns (->> nodes
                (map #(str/split % #"(\s*=\s*\()|(,\s*)|\)"))
                (map (juxt first rest))
                (into {}))]))

(defn find-exit [input]
  (let [[turns nodes] (parse-input input)]
    (loop [steps 0
           [this-step & next-steps] (cycle turns)
           here "AAA"]
      (println here)
      (if (= here "ZZZ")
        steps
        (recur (inc steps)
               next-steps
               (cond
                 (= this-step \R) (second (get nodes here))
                 (= this-step \L) (first (get nodes here))
                 :else (throw (Exception. (str "Not supposed to go here!: " this-step)))))))))

(find-exit sample-input-1)

(find-exit "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
")

(->> (slurp "/home/uusitalo/git/aoc/aoc2023/day8/input.txt")
     find-exit) ;; 22199
