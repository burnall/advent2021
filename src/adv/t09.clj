(ns adv.t09
  (:require [adv.util :refer [split-lines parse-long zip]]
            [clojure.string :refer [split]]))

(def input
  (->> "data/t09.txt"
       (slurp)
       (split-lines)
       (map parse-long)
       (vec)))

(defn initial-state [v pr-len]
  (->> v
       (take pr-len)
       (frequencies)))

(defn correct? [state n]
  (->> state
       (keys)
       (some #(and (state (- n %)) (not= % (- n %))))))

(defn next-state [state n prev]
  (let [m (if (= 1 (state prev)) 
            (dissoc state prev) 
            (update state prev dec))] 
    (if (m n)
      (update m n inc)
      (assoc m n 1))))

(defn find-mistake [v pr-len]
  (let [iter (fn [state i]
               (if (correct? state (v i))
                 (recur (next-state state (v i) (v (- i pr-len))) 
                        (inc i))
                 (v i)))]
    (iter (initial-state v pr-len)
          pr-len)))

(defn solve []
  (find-mistake input 25))

(defn sums-map [v]
  (->> (range)
       (zip (reductions + 0 v)) 
       (map vec)
       (into {})))  

(defn find-range [v n]
  (let [m (sums-map v)]
    (->> m
         (keys)
         (some (fn [a]
                 (when (m (- a n)) 
                   [(m a) (m (- a n))])))
         (sort))))

(defn handle-range [v min-i max-i]
  (->> (subvec v min-i max-i)
       ((juxt (partial apply min) (partial apply max)))))
                                    
(defn solve2 []
  (->> (solve)
       (find-range input)
       (apply handle-range input)))  

