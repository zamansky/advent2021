(ns day09
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))


(def data
  (->> 
   ;;(slurp "data/sample09.dat")
   (slurp "data/day09.dat")
   ;;(aocd/input 2021 9)
   (#(string/split % #"\n"))
   (map (fn [x] (map u/parse-int (string/split x #""))))
   (map  #(map-indexed (fn [i item] [i item]) %))
   (map-indexed (fn [j line]
                  (map (fn [[i k]] [j i k] ) line)
                  ) ) 
   (reduce (fn [r n] (into r n)) [] )
   )

  )

(def grid (reduce (fn [sofar [row col item]]
                    (assoc sofar [row col] item))
                  {} data
                  ))

(defn test-height [row col val]
  (if (or
       (>= val (get grid [(inc row) col] (inc val)))
       (>= val (get grid [(dec row) col] (inc val)))
       (>= val (get grid [row (inc col)] (inc val)))
       (>= val (get grid [row (dec col)] (inc val)))
       )

    false
    true

    ))

(defn part1 [grid]
(reduce (fn [result [row col]]
(if (test-height row col (get grid [row col]))
(conj result  [row col])
result
)) [] (keys grid)))

(apply + (map inc (map #(get grid %) (part1 grid))))
(count  (map #(get grid %) (part1 grid)))
;; 1841 too high as is 1757
