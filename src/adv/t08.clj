(ns adv.t08
  (:require [adv.util :refer [split-lines parse-int]]
            [clojure.string :refer [split]]))

(defn parse-command [s] s
  (let [[_ cmd arg] (first (re-seq #"(\S+) (.+)" s))]
    {:cmd (keyword cmd), :arg (parse-int arg)}))

(def input
  (->> "data/t08.txt"
       (slurp)
       (split-lines)
       (map parse-command)
       (vec)))

(defn run-next [memory state]
  (let [{:keys [pos hook acc]} state 
        {:keys [cmd arg]} (memory pos)
        [next-pos next-acc] (case cmd
          :acc [(inc pos) (+ acc arg)]
          :jmp [(+ pos arg) acc]
          :nop [(inc pos) acc])
        next-hook (hook next-pos)]
    (cond 
      (:stopped next-hook) {:stopped true, :acc next-acc} 
      (= next-pos (count memory)) {:acc next-acc}
      (or (< next-pos 0) (> next-pos (count memory))) {:error true}
      :else (run-next memory {:pos next-pos, :acc next-acc, :hook next-hook}))))

(defn get-hook [history]
  (fn [next-pos] 
    (if (history next-pos)
      {:stopped true}
      (hook (conj history next-pos)))))

(def initial-state
  {:pos 0, :acc 0, :hook (get-hook #{})})

(defn solve []
  (run-next input initial-state)) 

(defn adjust-and-run [program i] 
  (let [instr (program i)
        cmd (:cmd instr)
        adjusted (assoc-in program 
                           [i :cmd]
                           (if (= cmd :nop) :jmp :nop))]
    (run-next adjusted initial-state)))

(defn fix-program [program]
  (->> (count program)
       (range)
       (mapcat (fn [i] 
                 (condp = (:cmd (program i)) 
                   :nop [i]
                   :jmp [i]
                   nil)))
       (map (partial adjust-and-run program))
       (filter (fn [{:keys [stopped acc]}] (and (not stopped) acc (>= acc 0))))))

(defn solve2 []
  (fix-program input))

