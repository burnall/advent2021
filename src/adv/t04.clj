(ns adv.t04
  (:require [adv.util :refer [split-lines parse-int]]
            [clojure.string :refer [split]]))

(defn parse-pass [s]
  (->> (split s #"\s+")
       (map #(split % #":"))
       (into {}))) 

(defn parse-passports [s] 
  (->> (split s #"\n\n")
       (map parse-pass)))
 
(def input
  (-> "data/t04.txt"
      (slurp)
      (parse-passports)))

(defn pass-ok? [pass]
  (clojure.set/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} 
                       (set (keys pass))))

(defn solve 
  ([] (solve input))
  ([passports] 
    (->> passports 
         (filter pass-ok?)
         (count))))

