(ns adv.t12
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(defn parse-move [r]
  (let [[d & n] r]
    {:dir (keyword (clojure.string/lower-case (str d)))
     :dist (parse-int (apply str n))}))

(def input
  (->> "data/t12.txt"
       (slurp)
       (split-lines)
       (map parse-move)))

(defn travel [moves]
  (let [grads [[1 0] [0 1] [-1 0] [0 -1]]
        shift (fn [pos grad dist]
                (map + pos (map (partial * dist) (grads grad))))]
    (->> moves
         (reduce (fn [{:keys [grad pos]} {:keys [dir dist]}]
                   (case dir
                     :e {:grad grad, :pos (shift pos 0 dist)}
                     :s {:grad grad, :pos (shift pos 1 dist)}
                     :w {:grad grad, :pos (shift pos 2 dist)}
                     :n {:grad grad, :pos (shift pos 3 dist)}
                     :l (let [grad (mod (- grad (/ dist 90)) 4)]
                          {:grad grad, :pos pos}),  
                     :r (let [grad (mod (+ grad (/ dist 90)) 4)]
                         {:grad grad, :pos pos})
                     :f {:grad grad, :pos (shift pos grad dist)}))    
                 {:grad 0, :pos [0 0]})
         (:pos)))) 

(defn solve []
  (travel input))

(defn move-to-waypoint [pos wpos times]
  (->> wpos
       (map (partial * times))
       (map + pos)))

(defn rotate-waypoint [pos wpos angle]
  (let [q (mod (/ angle 90) 4)
        [wx wy] wpos]
    (case q
      0 [wx wy]
      1 [(- wy) wx]
      2 [(- wx) (- wy)]
      3 [wy (- wx)])))

(defn travel-waypoint [moves wpos]
  (let [grads [[1 0] [0 1] [-1 0] [0 -1]]
        shift (fn [pos grad dist]
                (map + pos (map (partial * dist) (grads grad))))]
    (->> moves
         (reduce (fn [{:keys [pos wpos]} {:keys [dir dist]}]
                   (case dir
                     :e {:pos pos, :wpos (shift wpos 0 dist)}
                     :s {:pos pos, :wpos (shift wpos 1 dist)}
                     :w {:pos pos, :wpos (shift wpos 2 dist)}
                     :n {:pos pos, :wpos (shift wpos 3 dist)}
                     :l {:pos pos, :wpos (rotate-waypoint pos wpos (- dist))}
                     :r {:pos pos, :wpos (rotate-waypoint pos wpos dist)} 
                     :f {:pos (move-to-waypoint pos wpos dist), :wpos wpos}))    
                 {:pos [0 0], :wpos wpos})
         (:pos))))

(defn solve2 []
  (travel-waypoint input [10 -1]))

