(ns adv.t07
  (:require [adv.util :refer [split-lines parse-int]]
            [clojure.string :refer [split]]))

(defn parse-inside [inside]
  (->> (split inside #",")
       (map (fn [s] 
              (let [[_ n bag] (first (re-seq #"(\d)+ (.+) bags?" s))]
                {:n (parse-int n), :bag bag})))))

(defn parse-rule [s]
  (let [[_ cont inside] (first (re-seq #"(.+) bags contain (.+)\." s))]
    (if (= inside "no other bags")
      [cont []]
      [cont (parse-inside inside)])))

(def input
  (->> "data/t07.txt"
       (slurp)
       (split-lines)
       (map parse-rule)
       (into {})))

(defn rev-containers [containers]
  (->> containers
       (mapcat (fn [[cont inside]]
                 (map (fn [{bag :bag}] [bag cont]) inside)))
       (reduce (fn [res [bag cont]]
                 (update res bag (fn [bags] (conj bags cont))))    
               {})))  

(defn reach-all [edges v-start]
   (let [iter (fn [vs-current vs-visited]
                (let [vs (->> vs-current
                              (mapcat edges)
                              (into #{})
                              (#(clojure.set/difference % vs-visited)))]
                   (if (empty? vs)
                     vs-visited
                     (recur vs (clojure.set/union vs-visited vs)))))
                             
         ]
     (iter [v-start] #{})))
                
(defn solve [] 
   (reach-all (rev-containers input)
              "shiny gold")) 

(defn sum-nested [edges v-start]
  (->> v-start
       (edges)
       (map (fn [{:keys [bag n]}]
              (* n (+ 1 (sum-nested edges bag)))))
       (reduce +)))

(defn solve2[] 
  (sum-nested input "shiny gold"))
