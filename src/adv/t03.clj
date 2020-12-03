(ns adv.t03
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)))

(defn solve
  ([] (solve input 3 1)) 
  ([forest dx dy] 
    (let [xmax (count (first forest))
          ymax (count forest)
          f (fn [x y] (get-in forest [y (mod x xmax)]))]
      (->> (quot ymax dy)
           (range 0)
           (filter #(= \# (f (* % dx) (* % dy))))
           (count)))))        

(defn solve2[]
  (reduce *
          [(solve input 1 1)
           (solve input 3 1)
           (solve input 5 1)
           (solve input 7 1)
           (solve input 1 2)]))  
