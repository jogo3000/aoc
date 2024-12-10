(ns day9)

(def long-sample "2333133121414131402")

(def short-sample "12345")

(def puzzle-input (slurp "day9/input"))

(defn parse-input [s]
  (let [padded-s (if (odd? (count s)) (str s "0") s)]
    (->> padded-s vec (map (comp (partial + -48) int)))))

(defn disk-map [blocks]
  (->> blocks
       (partition 2)
       (map-indexed (fn [n c] (cons n c)))
       (reduce (fn [acc [file-id length free]]
                 (-> acc
                     (into (repeat length file-id))
                     (into (repeat free nil)))) [])))

(defn vacuum [dm]
  (loop [dm dm]
    (if (nil? (peek dm))
      (recur (pop dm))
      dm)))

(defn compact [dm]
  (loop [dm (vacuum dm)
         i 0]
    (let [file-id (get dm i)]
      (cond
        (>= i (count dm)) dm
        (number? file-id) (recur dm (inc i))
        (nil? file-id) (recur (assoc (vacuum (pop dm)) i (peek dm)) (inc i))))))

(defn checksum [dm]
  (->> dm
       (map-indexed (fn [n c] (* n c)))
       (apply +)))

(->> long-sample parse-input disk-map compact checksum) ; 1928

(->> puzzle-input parse-input disk-map compact checksum) ; 6283404590840
