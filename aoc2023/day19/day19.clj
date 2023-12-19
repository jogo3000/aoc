(ns day19
  (:require [clojure.string :as str]))

(def sample "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
")

(defn parse-rule [rule]
  (let [rule-parts (str/split rule #":")
        target (last rule-parts)]
    (if (> (count rule-parts) 1)
      (let [rule (first rule-parts)
            id (str (get rule 0))
            gt-or-lt (get rule 1)
            evaluator (cond
                        (= gt-or-lt \>) >
                        (= gt-or-lt \<) <)
            reference (->> (subs rule 2) parse-long)]
        (fn [part]
          (when (evaluator (part id) reference) target)))
      (constantly target))))

(defn parse-input [input]
  (let [[wfs parts] (-> input str/trim (str/split #"\n\n"))
        wfs (into {}
                  (comp
                   (map (fn split-rules [s] (str/split s #"[\{\}]+")))
                   (map (fn [[id rules]]
                          [id (->> (str/split rules #",")
                                   (map parse-rule))])))
                  (str/split-lines wfs))

        parts (->> parts
                   str/split-lines
                   (map (fn [s]
                          (->> (str/split (subs s 1) #"[,=\}]+")
                               (partition 2)
                               (map vec)
                               (map #(update % 1 parse-long))
                               (into {})
                               ))))]
    [wfs parts]))

(defn evaluate-wfs [wfs part]
  (loop [dest "in"]
    (if (or (= "R" dest) (= "A" dest))
      dest
      (recur (let [wf (wfs dest)]
               (reduce (fn [_ f]
                         (when-let [dest (f part)]
                           (reduced dest))) nil wf))))))

(defn sum-accepted [input]
  (let [[wfs parts] (parse-input input)]
    (->> (get
          (->> parts
               (map (juxt (partial evaluate-wfs wfs) identity))
               (reduce (fn [acc [dest part]]
                         (update acc dest conj part)) {})) "A")
         (mapcat vals)
         (reduce +))))

(sum-accepted sample) ; 19114 - correct

(sum-accepted (slurp "day19/input.txt")) ; 399284

;; Part deux

(defn parse-rule2 [rule]
  (let [rule-parts (str/split rule #":")
        target (last rule-parts)]
    (if (> (count rule-parts) 1)
      (let [rule (first rule-parts)
            id (str (get rule 0))
            gt-or-lt (get rule 1)
            reference (->> (subs rule 2) parse-long)]
        [gt-or-lt id reference target])
      [:always target])))

(defn parse-input2 [input]
  (let [[wfs _] (-> input str/trim (str/split #"\n\n"))
        wfs (into {}
                  (comp
                   (map (fn split-rules [s] (str/split s #"[\{\}]+")))
                   (map (fn [[id rules]]
                          [id (->> (str/split rules #",")
                                   (map parse-rule2))])))
                  (str/split-lines wfs))]
    wfs))

(count (range 3999 4001))
(let [wfs (parse-input2 sample)
      pq {"x" [1 4000]
          "m" [1 4000]
          "a" [1 4000]
          "s" [1 4000]}]
  ((fn dfs [wf-id pq]
     (cond
       (= wf-id "R") 0
       (= wf-id "A") (->> pq vals (map (fn [[start end]] (- (inc end) start))) (reduce *))
       :else
       (let [wf (wfs wf-id)]
         (->> wf
              (reduce (fn [[ps pq] rule]
                        (cond (= (first rule) :always)
                              [(+ ps (dfs (second rule) pq))
                               {"x" [0 0] "m" [0 0] "a" [0 0] "s" [0 0]}]

                              ;; rfg{s<537:gd,x>2440:R,A}
                              (= (first rule) \>)
                              [(+ ps
                                  (dfs (last rule)
                                       (update pq (second rule) (fn [[start end]] [(max start (nth rule 2)) end]))))
                               (update pq (second rule) (fn [[start end]] [start (min end (dec (nth rule 2)))]))]

                              (= (first rule) \<)
                              [(+ ps (dfs (last rule)
                                          (update pq (second rule) (fn [[start end]] [start (min end (dec (nth rule 2)))]))))
                               (update pq (second rule) (fn [[start end]] [(max start (nth rule 2)) end]))]))
                      [0 pq])
              first))))
   "in" pq))

;;                167474394229030 overestimating, but why?
;; correct result 167409079868000
