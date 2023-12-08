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

(defn gcd [a b]
  (loop [a a
         b b]
    (if (zero? (mod (max a b) (min a b)))
      (min a b)
      (recur (min a b) (mod (max a b) (min a b))))))

(defn lcm [a b]
  (/ (* a b)
     (gcd a b)))

(defn least-common-multiple [xs]
  (reduce lcm xs))

(defn find-ghost-exit [s]
  (let [[turns nodes] (parse-input s)
        starting-nodes (->> nodes
                            (filter #(str/ends-with? (first %) "A"))
                            (map first))
        cycle-lengths
        (->> starting-nodes
             (map (fn [start]
                    (loop [steps 0M
                           [this-step & next-steps] (cycle turns)
                           loc start]
                      (if (str/ends-with? loc "Z")
                        steps
                        (recur (inc steps)
                               next-steps
                               (cond
                                 (= this-step \R) (second (get nodes loc))
                                 (= this-step \L) (first (get nodes loc))
                                 :else (throw (Exception. (str "Not supposed to go here!: " this-step))))))))))]
    (least-common-multiple cycle-lengths)))


(least-common-multiple [3 4 6])

(find-ghost-exit sample-input-3)

(find-ghost-exit
 (slurp "/home/uusitalo/git/aoc/aoc2023/day8/input.txt"))

;; Cycle lengths
;; (18827M 16579M 13207M 17141M 14893M 22199M) This will take time

;; 13334102464297M
