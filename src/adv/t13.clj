(ns adv.t13
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(defn parse-notes [notes]
  (let [[ts buses] notes
        buses (split buses #",")]
    {:ts (parse-int ts)
     :buses (->> buses
                 (filter (partial not= "x"))
                 (map parse-int))
     :rule (->> buses
                (zip (range))
                (filter (comp (partial not= "x") second))
                (map (fn [[i bus]] 
                        (when (not= bus "x") 
                          {:remainder i, :n (parse-int bus)}))))}))

(def input
  (->> "data/t13.txt"
       (slurp)
       (split-lines)
       (parse-notes)))

(defn waiting-time [ts bus]
  (let [wt (mod ts bus)]
    (mod (- bus wt) bus)))

(defn first-bus [{:keys [ts buses]}]
  (->> buses
       (apply min-key (partial waiting-time ts))))

(defn solve []
  (let [bus (first-bus input)]
     (* bus (waiting-time (:ts input) bus))))

(defn check-remainders [rule m]
  (->> rule
       (every? (fn [{:keys [remainder n]}]
                 (= remainder (waiting-time m n))))))

; Too ineffective
(defn find-ts [rule]  
  (let [[{:keys [remainder n]} & other-rules] (sort-by (comp - :n) rule)
        remainder (mod (- n remainder) n)] 
    (->> (range) 
         (map #(+ remainder (* n %)))
         (some (fn [m] (when (check-remainders other-rules m) m))))))

(defn lcm-form [[d1 r1] [d2 r2]]
  (let [[d1 r1 d2 r2] (if (< d1 d2) 
                        [d1 r1 d2 r2]
                        [d2 r2 d1 r1])]
    (->> (range d1)
         (map #(+ r2 (* d2 %)))
         (some (fn [x] (when (= (mod x d1) (mod r1 d1))
                         x)))
         ((fn [x] [(lcm d1 d2) x]))))) 

(defn find-ts2 [rule]
  (->> rule 
       (map (fn [{:keys [remainder n]}]
              [n (mod (- n remainder) n)]))
       (reduce lcm-form)))

(defn solve2 []
  (find-ts2 (:rule input)))
