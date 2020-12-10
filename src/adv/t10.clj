(ns adv.t10
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)
       (map parse-int)))

(defn get-diffs [adapters]
  (let [ads (sort adapters)]
    (->> ads
         (cons 0)
         (zip ads)
         (map (partial apply -))
         (frequencies))))

(defn solve []
  (let [{q1 1, q3 3} (get-diffs input)]
    (* q1 (inc q3))))


