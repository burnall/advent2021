(ns adv.t05
  (:require [adv.util :refer [split-lines parse-int]]
            [clojure.string :refer [split]]))

(def input
  (->> "data/t05.txt"
       (slurp)
       (split-lines)))

(defn pass-id [pass]
  (let [m {\F 0, \B 1, \L 0, \R 1}]
    (->> pass
         (map m)
         (apply str)
         (#(Integer/parseInt % 2)))))

(defn highest-id [passes]
  (->> passes
       (apply max-key pass-id)
       (pass-id)))

(defn solve []
  (highest-id input))

(defn find-seats [passes]
  (let [all (range (pass-id "BBBBBBBRRR")) 
        ids (set (map pass-id passes))]
    (->> all
         (filter #(and (not (ids %)) (ids (+ % 1)) (ids (- % 1)))))))

(defn solve2 [] 
  (find-seats input))

