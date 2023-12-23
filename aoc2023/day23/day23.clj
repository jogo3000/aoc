(ns day23
  (:require [clojure.string :as str])
  (:import [java.util ArrayDeque]))

(def sample-input (slurp "day23/sample.txt"))
(def puzzle-input (slurp "day23/input.txt"))

(def path \.)
(def forest \#)
(def east-slope \>)
(def west-slope \<)
(def north-slope \^)
(def south-slope \v)

(defrecord Map [m max-y max-x])

(defn parse-input [input]
  (let [m (->> input
               str/trim
               str/split-lines
               (mapv vec))
        max-y (dec (count m))
        max-x (dec (count (first m)))]
    (->Map m max-y max-x)))

(defn start [m]
  [0 1])

(defn end [m]
  [(:max-y m) (dec (:max-x m))])

(defn north [[y x]]
  [(dec y) x])

(defn south [[y x]]
  [(inc y) x])

(defn east [[y x]]
  [y (inc x)])

(defn west [[y x]]
  [y (dec x)])

(def dir->blocking-slope
  {north south-slope
   south north-slope
   east west-slope
   west east-slope})

(defn possible-moves [m visited pos]
  (remove visited ;; Don't repeat your steps
          ;; Can only go downward on a slope
          (condp = (get-in (:m m) pos)
            north-slope [(north pos)]
            south-slope [(south pos)]
            east-slope [(east pos)]
            west-slope [(west pos)]
            ;; else
            (into []
                  (comp
                   (map #(% pos))
                   (filter #(<= 0 (first %) (:max-y m))) ;; don't cross borders vert
                   (filter #(<= 0 (second %) (:max-y m))) ;; don't cross borders horizontally
                   (remove #(= (get-in (:m m) %) forest)) ;; don't step in to the forest. Don't!
                   )
                  [north south east west]))))

(def sample-map (parse-input sample-input))

;; At first I thought A* but let's try a recursive approach first
(defn find-scenic-route-recursive [m visited pos end]
  (if (= pos end) ;; At end?
    (count visited)
    (let [visited' (conj visited pos)]
      (->> (possible-moves m visited' pos)
           (reduce (fn [acc move]
                     (max acc (find-scenic-route-recursive m visited' move end)))
                   0)))))

(find-scenic-route-recursive sample-map #{}
                             (start sample-map)
                             (end sample-map)) ; 94 works for sample

(def puzzle-map (parse-input puzzle-input))

#_(find-scenic-route-recursive puzzle-map #{}
                             (start puzzle-map)
                             (end puzzle-map)) ;; causes a stack overflow

;; are there any crossroads on the map that do not include a slope?
(->> (for [y (range (inc (:max-y puzzle-map)))
           x (range (inc (:max-x puzzle-map)))
           :let [open-routes (possible-moves puzzle-map #{} [y x])]
           :when (and (= (get-in (:m puzzle-map) [y x]) path)
                      (> (count open-routes) 2))]
       [[y x] open-routes])
     (map (fn [[pos exits]] [pos (mapv #(get-in (:m puzzle-map) %) exits)]))
     (not-any? (fn [[_ exits :as all]]
                 (println exits all)
                 (some #(= % path) exits)))) ; true so map can be simplified

;; Adapt upper code to return crossroads
(defn crossroads [m]
  (for [y (range (inc (:max-y m)))
        x (range (inc (:max-x m)))
        :let [open-routes (possible-moves m #{} [y x])]
        :when (and (= (get-in (:m m) [y x]) path)
                   (> (count open-routes) 2))]
    [[y x] open-routes]))

(crossroads puzzle-map)

;; Ok this is fine, but let's assume we do find the connections. The amount of
;; _needed_ recursions is not lessened, because in the first recursive approach,
;; we don't need to go into a new stackframe when there's only one exit

(defn find-scenic-route-pessimistic-recursive [m visited pos end]
  (if (= pos end) ;; At end?
    (count visited)
    (let [visited' (conj visited pos)]
      (->> (possible-moves m visited' pos)
           (reduce (fn [acc move]
                     ;; follow path until we hit a crossroad where divergent
                     ;; paths are needed
                     (max acc
                          (loop [pos move
                                 visited visited']
                            (let [next-moves (possible-moves m visited pos)
                                  movescount (count next-moves)]
                              (cond
                                (zero? movescount) (inc ;; Need to account for the final move
                                                    (count visited)) ;; fizzle out, no possible moves
                                (= 1 movescount)                   ;; Follow path
                                (let [move (first next-moves)]
                                  (recur move (conj visited move)))
                                :else
                                (find-scenic-route-recursive m visited' pos end))))))
                   0)))))

(find-scenic-route-pessimistic-recursive
 sample-map
 #{}
 (start sample-map)
 (end sample-map)) ;; this works

#_(find-scenic-route-pessimistic-recursive
 puzzle-map
 #{}
 (start puzzle-map)
 (end puzzle-map)) ;; Still produces stack overflow

;; Ok nice try, but need to come up with something better that won't blow up the
;; stack

(defrecord Task [visited pos])

;; And that is a queue
(defn find-scenic-route-queued [m start end]
  (let [queue (ArrayDeque.)]
    (.add queue (->Task #{start} start))
    (loop [best 0]
      (if (.isEmpty queue)
        best
        (recur
         (max best
              (let [task (.pop queue)]
                ;; We still want to follow path to the next crossroads
                (loop [pos (:pos task)
                       visited (:visited task)]
                  (let [next-moves (possible-moves m visited pos)
                        movescount (count next-moves)]
                    (cond
                      (= end pos) (dec (count visited)) ;; dec to account for the starting pos in the visited list
                      (zero? movescount) 0 ;; Not in goal, checked before
                      (= 1 movescount) (let [move (first next-moves)]
                                         (recur move (conj visited move)))
                      :else
                      (do
                        (doseq [move next-moves]
                          (.add queue (->Task (conj visited move) move)))
                        0 ;; Not at goal yet!
                        )))))))))))

(find-scenic-route-queued sample-map
                          (start sample-map)
                          (end sample-map))

(find-scenic-route-queued puzzle-map
                          (start puzzle-map)
                          (end puzzle-map)) ; 2170 Correct!
