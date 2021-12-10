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
   ;;(slurp "data/day09.dat")
   (aocd/input 2021 9)
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

(def basin-points (part1 grid))

(defn find-neighbors [[row col] grid ]
  
  (let [val (get grid[row col] 9 )
        candidates [[(inc row) col]
                    [(dec row) col]
                    [row (inc col)]
                    [row (dec col)]    
                    ]
        final-candidates (filter #(let [v (get grid %  9)]
                                    (and
                                     (< v 9)
                                     (<  val v  )) ) candidates)
        ]
    final-candidates
    ))

(defn find-basin-size [[row col] grid]
  (loop [fronteir [[row col]]
         solution #{}
         grid grid 
         ]
    (let [current  (first fronteir)
          solution (into solution #{current})
          neighbors (find-neighbors current grid)
          neighbors (filter #(not (contains? solution %)) neighbors)
          fronteir (into (rest fronteir) neighbors)
          ]
      (cond
        (empty? fronteir)  solution
        :else (recur fronteir solution grid)
        ))))


(defn part2 [basin-points]
  (apply * (take 3 (reverse  (sort (map count (map #(find-basin-size % grid) basin-points)))))))
;; 1075536
