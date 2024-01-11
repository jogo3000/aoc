(ns day17)

(def puzzle-input (->> (slurp "day17/input.txt")
                       ((fn [s] (str "[" s "]")))
                       read-string))

(def max-bit (int (java.lang.Math/pow 2.0 20)))


(loop [queue (range max-bit)
       found 0]
  (println (count queue))
  (if (empty? queue)
    found

    (let [candidate (first queue)
          size
          (reduce (fn [acc b]
                    (when (> acc 150) (reduced acc))
                    (if (bit-test candidate b)
                      (+ acc (or (get puzzle-input b) 0))
                      acc)) 0 (range max-bit))
          fit? (= size 150)
          too-big? (> size 150)]
      (recur (if (or fit? too-big?)
               (remove (fn [n] (= (bit-and candidate n) candidate)) queue)
               (rest queue))
             (if fit? (inc found) found)))))

;; Part 2

(loop [queue (range max-bit)
       freqs {}]
  (println (count queue))
  (if (empty? queue)
    freqs

    (let [candidate (first queue)
          size
          (reduce (fn [acc b]
                    (when (> acc 150) (reduced acc))
                    (if (bit-test candidate b)
                      (+ acc (or (get puzzle-input b) 0))
                      acc)) 0 (range max-bit))
          fit? (= size 150)
          too-big? (> size 150)]
      (recur (if (or fit? too-big?)
               (remove (fn [n] (or (= (bit-and candidate n) candidate)
                                   (and fit?
                                        (> (Integer/bitCount n) (Integer/bitCount candidate))))) queue)
               (rest queue))
             (if fit? (update freqs (Integer/bitCount candidate) (fnil inc 0)) freqs)))))

; {5 5, 4 17}

;; This was slow but computer go brr
