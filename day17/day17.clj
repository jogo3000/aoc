(def sample-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def rock1 "####")

(def rock2
 ".#.
  ###
  .#.")


(def rock3
 "..#
  ..#
  ###")

(def rock4
 "#
  #
  #
  #")

(def rock5
 "##
  ##")

(def rock-order (cycle [rock1 rock2 rock3 rock4 rock5]))

(def winds (cycle sample-input))

(let [rock rock1]
  (loop [x 2
         y 3
         round 0]
    (if (zero? (mod round 2))
      (case (nth winds round)
        \> (let [new-x (inc x)
                 right-edge (+ new-x (or (clojure.string/index-of rock "\n") 1))]
             (recur (if (>= 7 right-edge) new-x x) y (inc round)))
        \< (let [new-x (- x)]
             (recur (max new-x 0) y (inc round)))
        (throw (ex-info "what??" {})))
      (if (zero? y) (list x y round)
          (recur x (dec y) (inc round))))))
