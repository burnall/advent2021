(ns adv.t17
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(defn parse-config [lines]
  (let [lines (mapv vec lines)
        getp (fn [x y] ((lines y) x))]
    (->> (for [x (range (count (lines 0)))
               y (range (count lines))]
           [x y])
         (reduce (fn [ps [x y]]
                   (if (= \# (getp x y))
                     (conj ps [x y 0])
                      ps)) 
                 #{})))) 

(def input
  (->> "data/t17.txt"
       (slurp)
       (split-lines)
       (parse-config)))

(def dirs3d 
  (for [x [0 -1 1]
        y [0 -1 1]
        z [0 -1 1]]
     [x y z]))

(defn adjacent [dirs excludeP p]
  (->> dirs
       (map (partial map + p))
       (#(if excludeP 
           (drop 1 %)
           %))))

(defn will-be-active? [dirs active-ps p]
  (->> (adjacent dirs true p)
       (filter active-ps)
       (count)
       ((fn [cnt] (or (= cnt 3) (and (= cnt 2) (active-ps p)))))))

(defn next-cycle [dirs active-ps]
  (->> active-ps
       (mapcat (fn [p] (conj (adjacent dirs false p) p)))
       (reduce (fn [{:keys [ps visited]} p]
                 (if (or (visited p) (not (will-be-active? dirs active-ps p))) 
                   {:ps ps, :visited visited}
                   {:ps (conj ps p), :visited (conj visited p)}))
               {:ps #{}, :visited #{}})
       (:ps)))

(defn run [dirs config]
  (->> config
       (iterate (partial next-cycle dirs))
       (drop 6)
       (first)
       (count)))

(defn solve []
  (run dirs3d input))

(def dirs4d 
  (for [x [0 -1 1]
        y [0 -1 1]
        z [0 -1 1]
        t [0 -1 1]]
     [x y z t]))

(def config4d
  (->> input
       (map (fn [p] (conj p 0)))
       (set)))

(defn solve2 []
  (run dirs4d config4d))

