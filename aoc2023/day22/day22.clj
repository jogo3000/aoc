(ns day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
")

(def puzzle-input (slurp "day22/input.txt"))

(defrecord Brick [id x1 y1 z1 x2 y2 z2])

(defn parse-brick [row]
  (let [[x1 y1 z1 x2 y2 z2] (->> (str/split row #"[,~]")
                                 (map parse-long))]
    (->Brick (str (gensym)) x1 y1 z1 x2 y2 z2)))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-brick)))

(defn sort-bricks [bricks]
  (sort-by #(min (:z1 %) (:z2 %)) bricks))

(defn x-extent [b]
  (set (range (min (:x1 b) (:x2 b))
              (inc (max (:x1 b) (:x2 b))))))

(defn y-extent [b]
  (set (range (min (:y1 b) (:y2 b))
              (inc (max (:y1 b) (:y2 b))))))

(defn z-extent [b]
  (set (range (min (:z1 b) (:z2 b))
              (inc (max (:z2 b) (:z2 b))))))

(defn bricks-overlap? [b1 b2]
  (and (seq (set/intersection (x-extent b1)
                              (x-extent b2)))
       (seq (set/intersection (y-extent b1)
                             (y-extent b2)))
       (seq (set/intersection (z-extent b1)
                              (z-extent b2)))))

(defn drop-brick [bricks-at-rest brick]
  (loop [falling-brick brick]
    (if (or (= (:z1 falling-brick) 1)
            (= (:z2 falling-brick) 1))
      falling-brick
      (let [future-position (-> falling-brick
                                (update :z1 dec)
                                (update :z2 dec))]
        (if (some (partial bricks-overlap? future-position) bricks-at-rest)
          falling-brick
          (recur future-position))))))

(defn drop-bricks [bricks]
  (loop [bricks-in-air (->> bricks
                            sort-bricks)
         bricks-at-rest []]
    (if (empty? bricks-in-air)
      bricks-at-rest
      (recur (rest bricks-in-air)
             (conj bricks-at-rest
                   (drop-brick bricks-at-rest (first bricks-in-air)))))))

(defn find-safe-to-disintegrate [bricks]
 (let [brick-set (set bricks)]
   (loop [safe []
          queue (sort-bricks bricks)]
     (if (empty? queue)
       safe
       (let [brick (first queue)
             other-bricks (disj brick-set brick)]
         (recur
          (if (= other-bricks (set (drop-bricks other-bricks)))
            (conj safe brick)
            safe)
          (rest queue)))))))

(defn analyze-input [input]
  (->> (parse-input input)
       drop-bricks
       find-safe-to-disintegrate))

(count (analyze-input sample-input)) ; 5 <- ok

(count (analyze-input puzzle-input))
