(ns adv.t10
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)
       (map parse-int)))

(defn get-chain [adapters]
  (cons 0 (sort adapters)))

(defn get-diffs [chain]
  (->> chain
       (zip (rest chain))
       (map (partial apply -))
       (frequencies)))
 
(defn solve []
  (let [{q1 1, q3 3} (get-diffs (get-chain input))]
    (* q1 (inc q3))))

(def delta 3)

(defn break-in-clusters [xs]
  (->> xs
       (reduce (fn [{:keys [groups prev]} elem]
                 {:prev elem
                  :groups (if (>= (- elem prev) delta)
                            (conj groups [elem])
                            (conj (pop groups) (conj (peek groups) elem)))})   
               {:groups [[]]  :prev 0})
       (:groups)))

(defn scan
  ([[x & xs]] (scan xs x))  
  ([[x & xs] prev]
    (if x
      (let [d (- x prev)] 
        (cond
          (empty? xs) (if (<= d delta) 1 0)
          (> d delta) 0
          (= d delta) (recur xs x)
          :else (+ (scan xs x) (scan xs prev))))
      1)))
      
(defn calc-combinations [chain]
  (->> chain
       (break-in-clusters)
       (map scan)
       (reduce *)))

(defn solve2 []
  (calc-combinations (get-chain input)))
