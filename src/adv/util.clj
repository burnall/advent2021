(ns adv.util)

(defn parse-int [s] (Integer/parseInt s))

(defn parse-long [s] (Long/parseLong s))

(def split clojure.string/split)

(defn split-lines [text]
  (split text #"\n"))

(defn is-digit [c]
  (and (>= (int c) (int \0)) (<= (int c) (int \9))))

(defn- insert-at [xs i x]
  (let [[a b] (split-at i xs)]
    (vec (concat a [x] b))))

(defn- permutate [x comb]
  (map #(insert-at comb % x)
       (range (inc (count comb)))))

(defn permutations [xs] 
  (reduce (fn [combinations x] 
            (->> combinations
                 (map (partial permutate x))
                 (apply concat)))
           [[]]
           xs))

(defn zip [& cols]
  (apply map (fn [& args] (identity args)) cols))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b)))) 

