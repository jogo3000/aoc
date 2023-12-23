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

;; Part deux For this part, the slopes need to be ignored. So I need
;; another "possible moves" rule. Or do I? I can just replace all slopes with
;; paths

(defn remove-slopes [s]
  (str/replace s (re-pattern (str \[ \\ north-slope ;; Need to escape the ^ character, that's why the \\ is there
                                  south-slope
                                  east-slope
                                  west-slope \])) "."))

(def slopeless-sample-map (->> sample-input remove-slopes parse-input))

(find-scenic-route-queued slopeless-sample-map
                          (start slopeless-sample-map)
                          (end slopeless-sample-map)) ; 154, works

;; Now for the big fish

(def slopeless-puzzle-map (->> puzzle-input remove-slopes parse-input))

#_(find-scenic-route-queued slopeless-puzzle-map
                          (start slopeless-puzzle-map)
                          (end slopeless-puzzle-map)) ;; hangs, need more smarts


;; The first problem I noticed with the implementation that it probably spends
;; time evaluating paths that are proven to be pointless, it can never reach a
;; higher score. But how to identify that? Step away from the computer for a sec
;; to think

;; Is this a branch & cut algorithm? Well I'm gonna stop assigning tasks to the
;; queue if it is clear that they can't find a longer route. For evaluating that
;; I will greate a _fill_ based algorithm
(defn evaluate-route-potential [m visited pos]
  (loop [moves (possible-moves m visited pos)
         visited visited]
    (if (empty? moves)
      (count visited)
      (let [visited' (into visited moves)
            new-moves (remove visited moves)]
        (recur (mapcat (partial possible-moves m visited') new-moves) visited')))))

(evaluate-route-potential sample-map #{(start sample-map)} (start sample-map)) ; 213 but let's check that to make sure

(- ;; subtract from whole map
 (* (count (:m sample-map))
    (count (first (:m sample-map))))
 ;; all forests
 (count (filter #(= % forest) sample-input))) ; 213 yeah, seems to work

;; Now for the pessimistic queued route evaluator

(defn find-scenic-route-queued-pessimistic [m start end]
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
                          (let [visited' (conj visited move)]
                            ;; Only add if the route has potential to beat the best found
                            (when (>= (evaluate-route-potential m visited' move) best)
                              (.add queue (->Task visited' move)))))
                        0 ;; Not at goal yet!
                        )))))))))))

(find-scenic-route-queued-pessimistic slopeless-sample-map
                                      (start slopeless-sample-map)
                                      (end slopeless-sample-map)); 154, works

#_(find-scenic-route-queued-pessimistic slopeless-puzzle-map
                                      (start slopeless-puzzle-map)
                                      (end slopeless-puzzle-map)) ;; Hangs, increase smarts

;; we are probably looking at routes that can't reach the end as well In order
;; to avoid that the evaluator needs to check if the end position is included in
;; the route potential

(defn realistically-evaluate-route-potential [m visited pos end]
  (loop [moves (possible-moves m visited pos)
         visited visited]
    (if (empty? moves)
      (if (contains? visited end) ;; Check if end is included in the potential route
        (count visited)
        0)
      (let [visited' (into visited moves)
            new-moves (remove visited moves)]
        (recur (mapcat (partial possible-moves m visited') new-moves) visited')))))

;; make sure this works for a closed loop
(let [m (parse-input "#.###
#.##.
#..#.
##.##
.#..#
.##.#")
      start (start m)
      end (end m)]
  (realistically-evaluate-route-potential m
                                          #{start (north end)} ;; a blocked route
                                          start end)) ; 0 seems to work


(defn find-scenic-route-queued-more-pessimistic [m start end]
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
                          (let [visited' (conj visited move)]
                            ;; Only add if the route has potential to beat the best found
                            (when (> (realistically-evaluate-route-potential m visited' move end) best)
                              (.add queue (->Task visited' move)))))
                        0 ;; Not at goal yet!
                        )))))))))))

#_(find-scenic-route-queued-more-pessimistic puzzle-map
                                           (start puzzle-map)
                                           (end puzzle-map))

;; 2170 produces same output for the first puzzle, great. Don't think for a moment I did that as a mistake

#_(find-scenic-route-queued-more-pessimistic slopeless-puzzle-map
                                           (start slopeless-puzzle-map)
                                           (end slopeless-puzzle-map))

;; The engine needs more juice cap'n!

;; OK OK Fine, I will parse the map into a network, jeez

;; For this we want the crossroads, and their exits. Follow the exits until another crossroad is found. Edge length needs to be stored as well

(defrecord Edge [start end weight])

(defn to-edges [input]
  (let [m input
        start (start m)
        end (end m)
        forks (into #{[start [(south start)]] ;; start and one exit south
                      [end [(north end)]]     ;; end and one exit north
                      } (crossroads m))
        nodes (into #{} (map first forks))]
    (into #{}
          (mapcat (fn [[node exits]]
                    (mapv (fn [exit]
                            (loop [pos exit
                                   visited #{node exit}
                                   weight 1]
                              (if (= pos node) ;; found a loop, valuable info!
                                (->Edge node node weight)
                                (let [next-pos (first (possible-moves m visited pos))]
                                  (if
                                      ;; found a new connection
                                      (contains? nodes next-pos)
                                    (->Edge node next-pos (inc weight))

                                    ;; Keep following
                                    (recur next-pos (conj visited next-pos) (inc weight))))))) exits)))
          forks)))

(defrecord Exit [node weight])

(defrecord NetworkMap [network start end])

(defn to-network [input]
  (let [m (parse-input input)]
    (->NetworkMap
     (->> (to-edges m)
          (map (fn [e] {(:start e) [(->Exit (:end e) (:weight e))]}))
          (apply merge-with into))
     (start m)
     (end m))))

;; Let's see if we can find our way to the exit and count correct steps using
;; this

(defn find-scenic-route-recursive-network [network visited pos end weight]
  (if (= pos end) weight
      (let [exits ((:network network) pos)]
        (reduce (fn [acc exit]
                  (if (contains? visited (:node exit))
                    acc
                    (max acc
                         (find-scenic-route-recursive-network
                          network
                          (conj visited (:node exit))
                          (:node exit)
                          end
                          (+ weight (:weight exit))))))
                weight
                exits))))

(let [nw (to-network (remove-slopes sample-input))]
  (find-scenic-route-recursive-network nw #{} (:start nw) (:end nw) 0)) ; 154, works

;; Well, can _this_ handle the real puzzle? I think it is time to find out how
;; many loops there are

;; Make a graphviz again

(let [drawn? (atom #{})]
  (doseq [node
          (->> (to-network (remove-slopes puzzle-input))
               :network
               )]
    (doseq [exit (second node)]
      (let [start (first node)
            end (:node exit)
            relation #{start end}]
        (when-not (contains? @drawn? relation)
          (swap! drawn? conj relation)
          (println (format "node%dx%d" (ffirst node) (second (first node)))
                   " -- " (format "node%dx%d" (first (:node exit)) (second (:node exit)))
                   ";"
                   ))))))

;; Based on the output, the loops can be hard to detect visually

;; Well just try to do it recursively first
(let [nw (to-network (remove-slopes puzzle-input))
      start (:start nw)
      end (:end nw)]
  (find-scenic-route-recursive-network nw #{start} start end 0))
;; 6681 too high? This means that it either makes some routes not allowed or
;; counts the results wrong

;; But it is fast enough to do the job.

(count (filter (partial = \.) (remove-slopes puzzle-input)))
;; 9438 Is the amount of room in the map, so 6681 seems quite high. Though not impossibly so
