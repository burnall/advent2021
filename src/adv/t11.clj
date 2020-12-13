(ns adv.t11
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(defn parse-row [r]
  (mapv #(if (= % \L) :empty :floor) r))

(def input
  (->> "data/t11.txt"
       (slurp)
       (split-lines)
       (mapv parse-row)))

(defn model [area]
  (let [maxy (count area)
        maxx (count (area 0))
        f (fn [x y] ((area y) x))
        cnt-taken
          (fn [x y]
            (let [x0 (max 0 (dec x))
                  x1 (min maxx (+ x 2))
                  y0 (max 0 (dec y))
                  y1 (min maxy (+ y 2))]
              (->> (for [a (range x0 x1)
                         b (range y0 y1)
                         :when (and (= :taken (f a b)) (or (not= a x) (not= b y)))]
                     1)
                   (count))))]
    (mapv (fn [y] 
            (mapv (fn [x] 
                    (condp = (f x y) 
                        :empty (if (zero? (cnt-taken x y)) :taken :empty) 
                        :taken (if (> (cnt-taken x y) 3) :empty :taken)
                        :floor))  
                  (range maxx))) 
          (range maxy))))    

(defn find-stable-seating [model-fn area]
  (->> {:area area, :stable false}
       (iterate (fn [{area :area}]
                  (let [nxt (model-fn area)]
                    {:area nxt
                     :stable (= area nxt)})))
       (some #(when (:stable %) (:area %)))))


(defn count-taken [area]
  (->> area
       (map (comp count (partial filter #(= % :taken))))
       (reduce +)))

(defn solve []
  (->> input
       (find-stable-seating model)
       (count-taken)))

(defn model2 [area]
  (let [maxy (count area)
        maxx (count (area 0))
        f (fn [x y] ((area y) x))
        in? (fn [x y] (and (>= x 0) (< x maxx) (>= y 0) (< y maxy))) 
        taken? (fn [x y dx dy] 
                 (let [x (+ x dx)
                       y (+ y dy)]
                   (when (in? x y)
                     (condp = (f x y)
                       :taken true
                       :empty false
                       :floor (recur x y dx dy)))))
        cnt-taken (fn [x y]
                    (->> [[1 0] [0 1] [-1 0] [0 -1] [1 -1] [-1 1] [-1 -1] [1 1]]
                         (map (fn [[dx dy]] (taken? x y dx dy)))
                         (filter identity)
                         (count)))]
    (mapv (fn [y] 
            (mapv (fn [x] 
                    (condp = (f x y) 
                        :empty (if (zero? (cnt-taken x y)) :taken :empty) 
                        :taken (if (> (cnt-taken x y) 4) :empty :taken)
                        :floor))  
                  (range maxx))) 
          (range maxy))))    

(defn solve2 []
  (->> input
       (find-stable-seating model2)
       (count-taken)))

