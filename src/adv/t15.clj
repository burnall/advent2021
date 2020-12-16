(ns adv.t15 
  (:require [adv.util :refer [zip]])) 

(def input [2 20 0 4 1 17])

(defn initial-state [kernel]
  (->> (range)
       (drop 1)
       (zip kernel)
       (map vec)
       (drop-last)
       (into {})
       ((fn [m] {:usage m, :el (last kernel), :idx (count kernel)}))))


(defn generate [kernel]
  (->> kernel
       (initial-state)
       (iterate (fn [{:keys [usage el idx]}]
                  {:usage (assoc usage el idx)
                   :el (if-let [i (usage el)]
                          (- idx i)
                          0) 
                   :idx (inc idx)}))
       (map :el)
       (lazy-cat (drop-last kernel))))

(defn solve []
  (nth (generate input) (dec 30000000)))

