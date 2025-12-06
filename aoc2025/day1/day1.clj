(ns day1
  (:require [clojure.string :as str]))

(def sample "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")


(defn solve [input]
  (->> input str/trim str/split-lines
       (reduce
        (fn [{:keys [dial zeros]} instruction]
          (let [direction (first instruction)
                amount (->> (subs instruction 1) parse-long)
                new-dial
                (-> ((case direction \L - \R +) dial amount)
                    (mod 100))]
            {:dial new-dial
             :zeros (if (zero? new-dial) (inc zeros) zeros)}))
        {:dial 50
         :zeros 0})))

(solve (slurp "day1/input")) ; {:dial 69, :zeros 1100}

;; Part 2

(defn solve2 [input]
  (->> input str/trim str/split-lines
       (reduce
        (fn [{:keys [dial zeros]} instruction]
          (let [direction (first instruction)
                amount (->> (subs instruction 1) parse-long)]
            (case direction
              \L
              (loop [zeros zeros
                     dial dial
                     clicks amount]
                (if (zero? clicks) {:dial dial
                                    :zeros zeros}
                    (let [turns (min (if (zero? dial) 100 dial) clicks)
                          new-dial (mod (- dial turns) 100)]
                      (recur (if (zero? new-dial) (inc zeros) zeros)
                             new-dial
                             (- clicks turns)))))

              \R
              (loop [zeros zeros
                     dial dial
                     clicks amount]
                (if (zero? clicks) {:dial dial
                                    :zeros zeros}
                    (let [turns (min (- 100 dial) clicks)
                          new-dial (mod (+ dial turns) 100)]
                      (recur (if (zero? new-dial) (inc zeros) zeros)
                             new-dial
                             (- clicks turns))))))))
        {:dial 50
         :zeros 0})))


(solve2 sample) ; {:dial 32, :zeros 6}

(solve2 (slurp "day1/input")) ; {:dial 69, :zeros 6358}
