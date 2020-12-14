(ns adv.t14
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(defn parse-mask-cmd [s]
  (let [[_ mask] (first (re-seq #"mask = (.+)" s))]
    (->> mask
         (reverse)
         (reduce (fn [{:keys [bit and-mask or-mask]} ch]
                   (case ch
                     \0 {:bit (bit-shift-left bit 1), :and-mask and-mask, :or-mask or-mask}
                     \1 {:bit (bit-shift-left bit 1), :and-mask (bit-or and-mask bit), :or-mask (bit-or or-mask bit)}
                     \X {:bit (bit-shift-left bit 1), :and-mask (bit-or and-mask bit), :or-mask or-mask})) 
                 {:bit 1, :and-mask 0, :or-mask 0})
         (#(dissoc % :bit)))))      

(defn parse-mem-cmd [s]
  (let [[_ idx v] (first (re-seq #"mem\[(\d+)\] = (\d+)" s))]
    {:idx (parse-int idx), :v (parse-int v)}))

(defn parse-cmd [s]
  (if (.startsWith s "mask")
     (parse-mask-cmd s)
     (parse-mem-cmd s))) 

(def input
  (->> "data/t14.txt"
       (slurp)
       (split-lines)
       (map parse-cmd)))

(defn apply-mask [v {:keys [and-mask or-mask]}]
  (bit-or or-mask (bit-and v and-mask)))

(defn run [cmds]
  (->> cmds
       (reduce (fn [agg cmd]
                 (if (:and-mask cmd)
                   (assoc agg :last-mask cmd)
                   (assoc agg :memory 
                          (assoc (:memory agg) 
                                 (:idx cmd) 
                                 (apply-mask (:v cmd) (:last-mask agg))))))   
               {:memory {}, :last-mask nil})
       (:memory)))

(defn solve []
  (->> input
       (run)
       (vals)
       (reduce +)))

