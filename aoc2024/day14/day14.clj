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

;; part 2

(defn simulate-1-second [max-x max-y {:keys [px py vx vy]}]
  {:px (mod (+ px vx) max-x)
   :py (mod (+ py vy) max-y)
   :vx vx
   :vy vy})

(def repeating-figure
  (let [arrangement
        (->> (slurp "day14/input")
             parse-input)]
    (loop [arr arrangement
           states {}
           n 0]
      #_(println (count states))
      (cond
        (>= n 100000)
        :not-found

        (or #_(= 10402 n) (states arr))
        (do (println n)
            (def *states states)
            arr)

        :else
        (recur (map (partial simulate-1-second 101 103) arr)
               (assoc states arr n)
               (inc n)))))) ; 10403 is a point when we see something repeating

;; Upper bound is 10403 then

(map second (sort-by second *states))

(defn render-figure [figure]
  (->>
   (let [mapp (vec
               (for [i (range 104)]
                 (vec
                  (for [j (range 102)]
                    \.))))]
     (reduce (fn [m {:keys [px py]}]
               (assoc-in m [py px] \0)) mapp figure))
   (map str/join)
   (str/join "\n")))

(println (render-figure repeating-figure))

(println (render-figure (ffirst *states)))

(run!
 (fn [[state n]]
   (spit "tree.txt" (str "------ " n " ------\n") :append true)
   (spit "tree.txt" (render-figure state) :append true))
 (sort-by second *states))

;; So the output surely contains the image. Upon inspecting the image there is a
;; repeating pattern every 101 renderings, starting from frame 11. Scanning the
;; renderings can find the answer which is 6475 (/ (- 6475 11) 101) = 64 cycles
