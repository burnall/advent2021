(ns adv.t19
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(defn parse-ints [s]
  (->> (split s #" ")
       (map parse-int)))
        
(defn parse-rule [s]
  (let [[_ n ending] (re-find #"(\d+): (.+)" s)]
    {:n (parse-int n)
     :rule (if (.startsWith ending "\"") 
              (re-find #"\w+" ending)
              (->> (split ending #" \| ")
                   (map parse-ints)))}))

(defn parse-input [s]
  (let [[rules-str msgs-str] (split s #"\n\n")]
    {:rules (->> rules-str
                 (split-lines)
                 (map parse-rule)
                 (sort-by :n)
                 (mapv :rule))
     :msgs (split-lines msgs-str)}))  

(def input
  (->> "data/t19.txt"
       (slurp)
       (parse-input)))

(defn make-struct [rules])
