(ns adv.t01
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)
       (map parse-int)))

(defn find-in-set [s sum]
  (->> s
       (filter #(s (- sum %)))
       (first))) 

(defn solve
  ([] (solve input))
  ([xs] 
    (let [s (set xs)]
      (-> s
          (find-in-set 2020)
          (#(* % (- 2020 %))))))) 

(defn solve2
  ([] (solve2 input))
  ([xs]
    (let [s (set xs)]
      (->> s
           (some (fn [i] 
                   (if-let [a (find-in-set s (- 2020 i))]
                     [i a (- 2020 a i)])))
           (reduce *)))))


