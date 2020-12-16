(ns adv.t16
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(defn parse-ints [sep s]
  (mapv parse-int 
        (split s (re-pattern sep)))) 

(defn parse-req [req]
  (let [[_ param ranges] (first (re-seq #"(.+): (.+)" req))]
    (->> (split ranges #" or ")
         (map (partial parse-ints "-"))
         (assoc {:param param} :ranges))))

(defn parse-reqs [reqs]
  (->> reqs
       (split-lines)
       (map parse-req)))

(defn parse-ticket [s]
  (-> s
      (split-lines)
      (second)
      (#(parse-ints "," %)))) 

(defn parse-tickets [note]
  (->> note
       (split-lines)
       (drop 1)
       (map (partial parse-ints ","))))

(defn parse-notes [notes] 
  {:reqs (parse-reqs (notes 0))
   :ticket (parse-ticket (notes 1))
   :nearby-tickets (parse-tickets (notes 2))})

(def input
  (-> "data/t16.txt"
      (slurp)
      (split #"\n\n")
      (parse-notes)))

(defn fit-req? [v {ranges :ranges}]
  (->> ranges 
       (some (fn [[mi ma]] (and (>= v mi) (<= v ma))))))
 
(defn valid-value? [reqs v]
  (some (partial fit-req? v) reqs))
  
(defn find-invalid-values [{:keys [reqs nearby-tickets]}]
  (->> nearby-tickets
       (mapcat identity)
       (filter (comp not (partial valid-value? reqs)))
       (reduce +)))

(defn solve []
  (find-invalid-values input))

(defn possible-params [reqs v]
  (->> reqs
       (filter (partial fit-req? v))
       (map :param)
       (into #{})))

(defn collect [{:keys [reqs nearby-tickets ticket]}]
  (->> nearby-tickets
       (filter (partial 
                 every? (fn [v] (some (partial fit-req? v) reqs))))
       (map (partial mapv (partial possible-params reqs)))
       ))

(defn interpret-by-index [tickets]
  (->> (count (first tickets))
       (range)
       (map (fn [i]
              (apply clojure.set/intersection 
                     (map (fn [ticket] (ticket i)) 
                          tickets))))
       (zip (range))
       (sort-by (comp count second))
       (reduce (fn [{:keys [prev res] } [i params]]
                 {:prev params 
                  :res (assoc res i (first (clojure.set/difference params prev)))})                 
               {:prev #{}, :res {}})
       (:res)))
               
(defn solve2 []
  (let [ticket (:ticket input)
        index-to-param (interpret (collect input))
        param-to-index (->> index-to-param
                            (map (comp vec reverse))
                            (into {}))]
     (->> param-to-index
          (keys)
          (filter #(.startsWith % "departure"))
          (map (fn [param]
                 (ticket (param-to-index param))))  
        )))
