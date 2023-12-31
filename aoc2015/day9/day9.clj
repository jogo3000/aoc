(ns day9)

;; Traveling salesman problem with 28 cities, brute force is not going to fly.

(let [n 28]
  (* (Math/pow n 2) (Math/pow 2 n)))
