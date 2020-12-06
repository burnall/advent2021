(ns adv.t06
  (:require [adv.util :refer [split-lines parse-int]]
            [clojure.string :refer [split]]))

(defn parse-answers [s] 
  (->> (split s #"\n\n")
       (map split-lines)))
 
(def input
  (-> "data/t06.txt"
      (slurp)
      (parse-answers)))

(defn merge-group [group]
  (->> group
       (mapcat seq)
       (into #{})))

(defn total-count [groups handle-group]
  (->> groups
       (map handle-group)
       (map count)
       (reduce +)))

(defn solve []
  (total-count input merge-group))

(defn intersect-in-group [group]
  (->> group
       (map set)
       (apply clojure.set/intersection)))

(defn solve2 [] 
  (total-count input intersect-in-group))
