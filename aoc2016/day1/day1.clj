(ns day1
  (:require [clojure.string :as str]))

(def sample-1 "R2, L3")

(def puzzle-input "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3")

(def dirs {[1 0] {"R" [0 1]
                  "L" [0 -1]}
           [0 1] {"R" [-1 0]
                  "L" [1 0]}
           [-1 0] {"R" [0 -1]
                   "L" [0 1]}
           [0 -1] {"R" [1 0]
                   "L" [-1 0]}}) ;; This mapping is likely not right


(defn solve [puzzle-input]
  (let [steps (-> puzzle-input
                  (str/split #",\s+"))]
    (loop [steps steps
           dir [1 0]
           coords [0 0]]
      (println coords)
      (let [head (first steps)
            tail (rest steps)]
        (if-not head
          (->> coords (map abs) (reduce +))
          (let [turn (subs head 0 1)
                amount (-> head (subs 1) parse-long)
                new-dir ((dirs dir) turn)]
            (recur tail
                   new-dir
                   [(+ (first coords) (* amount (first new-dir)))
                    (+ (second coords) (* amount (second new-dir)))])))))))


(solve sample-1) ; 5

(solve "R2, R2, R2") ; -2

(solve "R5, L5, R5, R3") ; 12

(solve "R5, R5, R5, R5")


(solve puzzle-input) ; 243

;; Part 2
(defn solve [puzzle-input]
  (let [steps (-> puzzle-input
                  (str/split #",\s+"))]
    (loop [steps steps
           dir [1 0]
           coords [0 0]
           visited #{}
           done false]
      (let [head (first steps)
            tail (rest steps)]
        (if (or (not head) done)
          (->> coords (map abs) (reduce +))
          (let [turn (subs head 0 1)
                amount (-> head (subs 1) parse-long)
                new-dir ((dirs dir) turn)]
            (letfn [(step [coords]
                      [(+ (first coords) (first new-dir))
                       (+ (second coords) (second new-dir))])]
              (let [path
                    (->> coords
                         (iterate step)
                         (take (inc amount))
                         (drop 1)
                         #_(into [coords]))
                    crossing? (some visited path)]
                (recur tail
                       new-dir
                       (if crossing? crossing? (last path))
                       (into visited path)
                       (if crossing? true false))))))))))


(solve "R8, R4, R4, R8")
(solve puzzle-input) ; 142
