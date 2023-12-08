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


;; part 2
(def sample-input-3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
")

(defn find-ghost-exit [s]
  (let [[turns nodes] (parse-input s)
        starting-nodes (->> nodes
                            (filter #(str/ends-with? (first %) "A"))
                            (map first))]
    (loop [steps 0
           turns turns
           locations starting-nodes]
      (let [this-step (first turns)
            next-steps (next turns)]
        (if (every? #(str/ends-with? % "Z") locations)
          steps
          (recur (inc steps)
                 (if (empty? next-steps)
                   turns
                   next-steps)
                 (for [loc locations]
                   (cond
                     (= this-step \R) (second (get nodes loc))
                     (= this-step \L) (first (get nodes loc))
                     :else (throw (Exception. (str "Not supposed to go here!: " this-step)))))))))))

(find-ghost-exit
 (slurp "/home/uusitalo/git/aoc/aoc2023/day8/input.txt"))
