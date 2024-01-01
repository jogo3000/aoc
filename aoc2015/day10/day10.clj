(ns day10
  (:require [clojure.string :as str])
  (:import [java.math MathContext]))

(defn look-and-say [s]
  (loop [[c & s] s
         s' ""
         last-c nil
         l 0]
    (cond
      (not c)
      (str s' l last-c)

      (not= last-c c)
      (recur s (str s'
                    (when (pos? l)
                      (str l last-c))) c 1)
      :else (recur s s' last-c (inc l)))))

(look-and-say "111221")

(-> (iterate look-and-say "1113122113")
    (nth 40)
    count); 360154


;; Part two

#_(-> (iterate look-and-say "1113122113")
    (nth 50)
    count)

;; Takes very long!

;; The puzzle recommends a video where Conway explains the sequence

;; Let's try to apply the Conway's constant here

(def conway's-constant
  (BigDecimal. (str "1." (str/join '(3, 0, 3, 5, 7, 7, 2, 6, 9, 0, 3, 4, 2, 9, 6, 3, 9, 1, 2, 5, 7, 0, 9, 9, 1, 1, 2, 1, 5, 2, 5, 5, 1, 8, 9, 0, 7, 3, 0, 7, 0, 2, 5, 0, 4, 6, 5, 9, 4, 0, 4, 8, 7, 5, 7, 5, 4, 8, 6, 1, 3, 9, 0, 6, 2, 8, 5, 5, 0, 8, 8, 7, 8, 5, 2, 4, 6, 1, 5, 5, 7, 1, 2, 6, 8, 1, 5, 7, 6, 6, 8, 6, 4, 4, 2, 5, 2, 2, 5, 5, 5)))))

(defn conway's-look-and-say "Produces length of the next sequence" [l]
  (.toBigInteger (.multiply conway's-constant (BigDecimal. l))))


(nth (iterate conway's-look-and-say (BigDecimal. 360154)) 10)

; 6652621 ;; too high



;; 5103357.95878281
;; Still too low, after rounding up to full integer

;; 5103362
;; too low probably due to rounding errors


(nth (iterate conway's-look-and-say (count "111312213")) 40) ; 291436 This again loses information too much

;; Maybe I should use the "Elements" from Conway's paper. Ugh, not now however

;; One more shot with iterating but constructing the strings more intelligently before that


(defn faster-look-and-say [s]
  (loop [[c & s] s
         s' '()
         last-c nil
         l 0]
    (cond
      (not c)
      (let [s'' (if (pos? l) (cons last-c (cons l s'))
                    s')]
        (str/join (reverse s'')))

      (not= last-c c)
      (recur s (if (pos? l)
                 (cons last-c (cons l s'))
                 s') c 1)
      :else (recur s s' last-c (inc l)))))


(take 10 (iterate faster-look-and-say "1"))

'("1"
  "11"
  "21"
  "1211"
  "111221"
  "312211"
  "13112221"
  "1113213211"
  "31131211131221"
  "13211311123113112211")

(take 10 (iterate look-and-say "1"))

'("1"
  "11"
  "21"
  "1211"
  "111221"
  "312211"
  "13112221"
  "1113213211"
  "31131211131221"
  "13211311123113112211")

(let [n 15
      s "1113122113"]
  (= (take n (iterate faster-look-and-say s))
     (take n (iterate look-and-say s))))

(count (nth (iterate faster-look-and-say "1113122113") 50)) ; 5103798
