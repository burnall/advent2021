(ns adv.t18
  (:require [adv.util :refer [split-lines parse-int zip lcm]]
            [clojure.string :refer [split]]))

(def spec-tokens
  {"+" :plus
   "*" :mul
   "(" :lparen
   ")" :rparen})

(def operators 
  {:plus +, :mul *})

(defn read-operand [[token & more-tokens]]
  (if (= token :lparen)
     (let [{:keys [ast more-tokens]} (build-ast more-tokens)]
       [ast (rest more-tokens)])
     [token more-tokens]))

(defn read-right-operands [left-operand operator tokens]
  (let [[right-operand more-tokens] (read-operand tokens)
        next-token (first more-tokens)
        ast {:oper operator, :l-ast left-operand, :r-ast right-operand}]
    (if (operators next-token)
      (recur ast next-token (rest more-tokens))
      {:ast ast, :more-tokens more-tokens})))

(defn build-ast [tokens]
  (let [[left-operand more-tokens] (read-operand tokens)
        [operator & more-tokens] more-tokens]
    (read-right-operands left-operand operator more-tokens))) 

(defn parse-expr [s]
  (let [tokens (re-seq #"\d+|\+|\*|\(|\)" s)]
    (->> tokens
         (map (fn [t] (or (spec-tokens t) (parse-int t)))))))

(def raw-input
  (->> "data/t18.txt"
       (slurp)
       (split-lines)))

(def input
  (->> raw-input
       ;(drop 1)
       ;(take 1)
       (map parse-expr)))

(defn evaluate [ast]
   (cond 
     (int? ast)  ast
     (:oper ast) ((operators (:oper ast)) (evaluate (:l-ast ast)) 
                                          (evaluate (:r-ast ast)))
     :else (assert false)))          

(defn solve []
  (->> input
       (map build-ast)
       (map :ast)
       (map evaluate)
       (reduce +)))
       ;(zip raw-input)))

(declare build-ast2)

(defn read-operand2 [priorities [token & more-tokens]]
  (if (= token :lparen)
     (let [{:keys [ast more-tokens]} (build-ast2 priorities more-tokens)]
       [ast (rest more-tokens)])
     [token more-tokens]))

(defn read-form [priorities operands ops tokens]
  (let [[operand more-tokens] (read-operand2 priorities tokens)
        next-token (first more-tokens)]
    (if (operators next-token)
      (recur priorities
             (conj operands operand)
             (conj ops next-token)   
             (rest more-tokens))
      {:operands (conj operands operand), :ops ops, :more-tokens more-tokens}))) 

(defn operands-to-ast [priorities {:keys [operands ops]}]
  (if (empty? ops)
    (first operands)
    (let [op-index (->> ops
                        (zip (range))
                        (sort-by (comp priorities second))
                        (first)
                        (first))
          operands (-> (concat (subvec operands 0 op-index)
                               [{:oper (ops op-index), :l-ast (operands op-index), :r-ast (operands (inc op-index))}]
                               (subvec operands (+ op-index 2)))
                       (vec))
          ops (->> (concat (subvec ops 0 op-index)
                           (subvec ops (inc op-index)))
                   (vec))]
      (recur priorities {:operands operands, :ops ops}))))     

(defn build-ast2 [priorities tokens]
  (let [form (read-form priorities [] [] tokens)]
    {:ast (operands-to-ast priorities form)
     :more-tokens (:more-tokens form)}))

(defn solve2 
  ([] (solve2 {:plus 0, :mul 10}))
  ([priorities]
     (->> input
          (map (partial build-ast2 priorities)) 
          (map :ast) 
          (map evaluate)
          (reduce +)
          )))

