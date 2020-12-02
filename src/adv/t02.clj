(ns adv.t02
  (:require [adv.util :refer [split-lines parse-int]]))


(defn parse-rule [s] 
  (let [[_ a b ch text] (first (re-seq #"(\d+)-(\d+) (\S): (.+)" s))]
    {:a (parse-int a)
     :b (parse-int b) 
     :ch (first ch)
     :text text}))

(def input
  (->> "data/t02.txt"
       (slurp)
       (split-lines)
       (map parse-rule)))

(defn rule-fit? [r]
  (let [{:keys [a b ch text]} r
        cnt (->> text
                 (filter (partial = ch))
                 (count))]
    (and (>= cnt a) (<= cnt b))))

(defn solve
  ([] (solve rule-fit? input))
  ([fit? rules] 
    (->> rules
         (filter fit?)
         (count))))

(defn rule-fit2? [r]
  (let [{:keys [a b ch text]} r
        ch1 (get text (dec a))
        ch2 (get text (dec b))]
    (->> [ch1 ch2]
         (filter (partial = ch))
         (count)
         (= 1))))

