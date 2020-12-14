(ns adv.t14
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(defn parse-mask-cmd [s]
  (let [[_ mask] (first (re-seq #"mask = (.+)" s))]
    (->> mask
         (reverse)
         (reduce (fn [{:keys [bit and-mask or-mask]} ch]
                   {:bit (bit-shift-left bit 1), 
                    :and-mask (if (= ch \0) 
                                and-mask
                                (bit-or and-mask bit))
                    :or-mask (if (= ch \1) 
                               (bit-or or-mask bit)
                               or-mask)})
                 {:bit 1, :and-mask 0, :or-mask 0, :raw s})
         (#(dissoc % :bit))
         (#(assoc % :raw mask)))))    

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

(defn next-values [vs v]
  (map (partial bit-or v) vs))

(defn apply-mask2 [v {raw :raw}]
  (->> raw
       (reverse)
       (reduce (fn [{:keys [bit idx vs]} ch]
                 {:idx (inc idx)
                  :bit (bit-shift-left bit 1)
                  :vs (case ch
                        \0 (if (bit-test v idx) (next-values vs bit) vs)
                        \1 (next-values vs bit)
                        \X (concat vs (next-values vs bit)))})
               {:bit 1, :idx 0, :vs [0]})
       (:vs)))

(defn memory-change [indexes v]
  (->> indexes 
       (map (fn [idx] [idx v]))
       (into {})))

(defn run2 [cmds]
  (->> cmds
       (reduce (fn [agg cmd]
                 (if (:raw cmd)
                   (assoc agg :last-mask cmd)
                   (assoc agg :memory 
                          (merge (:memory agg)
                                 (memory-change (apply-mask2 (:idx cmd) (:last-mask agg)) 
                                                (:v cmd))))))
               {:memory {}, :last-mask nil})
       (:memory)))

(defn solve2 []
  (->> input
       (run2)
       (vals)
       (reduce +)))

