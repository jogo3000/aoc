(ns day18
  (:require [clojure.string :as str]))

(def sample "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
")

(defn parse-input [input]
  (->> input
       str/trim
       str/split-lines
       (map #(let [[d n c] (str/split % #"\s+")]
               [d (parse-long n) c]))))

(defn follow [d [y x]]
  (case d
    "U" [(dec y) x]
    "D" [(inc y) x]
    "L" [y (dec x)]
    "R" [y (inc x)]))

(defn make-trench [instructions]
  (loop [trench [[0 0]]
         instructions instructions
         position [0 0]]
    (if (empty? instructions)
      trench
      (let [[[d n _] & remaining] instructions
            [new-trench new-pos]
            (->> (range n)
                 (reduce (fn [[trench [y x]] _]
                           (let [new-pos (follow d [y x])]
                             [(conj trench new-pos) new-pos])) [trench position]))]
        (recur new-trench remaining new-pos)))))

(defn extent [trench]
  (reduce (fn [[y1 x1 y2 x2] [y x]]
            [(min y1 y) (min x1 x) (max y2 y) (max x2 x)]) [Long/MAX_VALUE
            Long/MAX_VALUE
            Long/MIN_VALUE
            Long/MIN_VALUE] trench))

(defn direction [[y x] [y' x']]
  (cond
    (and (= y y') (= (inc x) x')) "R"
    (and (= y y') (= (dec x) x')) "L"
    (and (= (inc y) y') (= x x')) "D"
    (and (= (dec y) y') (= x x')) "U"
    :else (throw (Exception. (str "Should not happen!" [y x] [y' x'])))))

(defn right-to [pos dir]
  (follow
   (case dir
     "R" "D"
     "L" "U"
     "D" "L"
     "U" "R") pos))

(defn some-interior-cells [trench]
  (let [trench-cells (set trench)]
    (disj (into #{}
                (mapcat identity)
                (for [[p p'] (partition 2 1 trench)
                      :let [d (direction p p')
                            r (right-to p d)
                            r' (right-to p' d)]]
                  [(when-not (trench-cells r) r)
                   (when-not (trench-cells r') r')])) nil)))

(defn fill-trench [trench]
  (let [[miny minx maxy maxx] (extent trench)
        trench-cells (set trench)
        interior-cells
        (loop [covered #{}
               queue (into '() (some-interior-cells trench))]
          (if (empty? queue)
            covered
            (recur (conj covered (first queue))
                   (into (rest queue)
                         (filter #(and (not (trench-cells %))
                                       (not (covered %))
                                       (<= miny (first %) maxy)
                                       (<= minx (second %) maxx))
                                 (let [head (first queue)]
                                   [(follow "U" head)
                                    (follow "D" head)
                                    (follow "L" head)
                                    (follow "R" head)]))))))]
    (into trench-cells interior-cells)))


(defn visualize [dugout]
  (let [[miny minx maxy maxx] (extent dugout)
        dset (set dugout)]
    (str "------\n"
         (str/join "\n"
                   (for [y (range miny (inc maxy))]
                     (str/join
                      (for [x (range minx (inc maxx))]
                        (if (contains? dset [y x])
                          \#
                          \.))))))))

(defn measure-trench [input]
  (->> (parse-input input)
       make-trench
       fill-trench
       count))

(measure-trench sample) ; 62

(measure-trench (slurp "day18/input.txt")) ; 40745
