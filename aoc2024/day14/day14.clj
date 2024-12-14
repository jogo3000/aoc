(ns day14
  (:require [clojure.string :as str]))

(def sample-data
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")


(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #"\s"))
       (map (fn [parts]
              (let [[p v] (map #(str/split (subs % 2) #",") parts)
                    [px py]
                    (map parse-long p)
                    [vx vy]
                    (map parse-long v)]
                {:px px :py py :vx vx :vy vy})))))

(defn simulate-100-seconds [max-x max-y {:keys [px py vx vy]}]
  {:x (mod (+ px (* 100 vx)) max-x)
   :y (mod (+ py (* 100 vy)) max-y)})


(defn simulate-and-find-quadrants [input max-x max-y]
  (->> input parse-input
       (map (partial simulate-100-seconds max-x max-y))
       (group-by (fn [{:keys [x y]}]
                   (let [mid-x (int (/ max-x 2))
                         mid-y (int (/ max-y 2))]
                     (cond
                       (and (< x mid-x)
                            (< y mid-y)) :top-left
                       (and (> x mid-x)
                            (< y mid-y)) :top-right
                       (and (< x mid-x)
                            (> y mid-y)) :low-left
                       (and (> x mid-x)
                            (> y mid-y)) :low-right
                       :else
                       :middle))))))

(simulate-and-find-quadrants sample-data 11 7)

(defn safety-factor [quadrants]
  (->> (dissoc quadrants :middle)
       vals
       (map count)
       (apply *)))

(->> (simulate-and-find-quadrants sample-data 11 7)
     safety-factor)

(->> (simulate-and-find-quadrants (slurp "day14/input") 101 103)
     safety-factor) ; 231782040
