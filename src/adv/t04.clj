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

(defn check-passports [passports check?]
  (->> passports 
       (filter check?)
       (count)))

(defn solve [] 
  (check-passports input pass-ok?))

(defn to-num [s]
  (if (and s (some? (re-seq #"^\d+$" s)))
    (parse-int s)
    -1))

(defn pass-ok-two? [pass]
  (let [birth-year (to-num (get pass "byr"))
        issue-year (to-num (get pass "iyr"))
        expiration-year (to-num (get pass "eyr"))
        hair (get pass "hcl" "")
        eye (get pass "ecl" "")
        passport (get pass "pid" "")
        [_ height-s unit] (first (re-seq #"(\d+)(in|cm)" (get pass "hgt" "")))
        height (to-num height-s)] 
    (and (>= birth-year 1920) (<= birth-year 2002)
         (>= issue-year 2010) (<= issue-year 2020)
         (>= expiration-year 2020) (<= expiration-year 2030)
         (or (and (= unit "cm") (>= height 150) (<= height 193))
             (and (= unit "in") (>= height 59) (<= height 76)))
         (some? (re-seq #"^\d{9}$" passport))
         (some? (re-seq #"^#[0-9a-f]{6}$" hair))
         (some? (re-seq #"^(amb|blu|brn|gry|grn|hzl|oth)$" eye)))))

(defn solve2 []
  (check-passports input pass-ok-two?))

