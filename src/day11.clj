(ns day11
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))
(def data
  (->> 
   ;;(slurp "data/sample11.dat")
   (aocd/input 2021 11)
   ( #(string/split % #"\n"))
   ))

(def cols (count (first data)))
(def rows (count data))
(def board (into {} (map-indexed

                     (fn [row line]
                       (into {}  (map-indexed (fn [col char]
                                                {[row col] (-  (int char) 48)} 
                                                ) line))
                       )
                     data ))
  )


(def neighbordeltas (for [x [-1 0 1] y [-1 0 1]
                          :when (not= 0 x y)] [x y]))

(def cell-bloc-3x3 (for [x [-1 0 1] y [-1 0 1]] [x y]))

(defn get-neighbor-coords [ [r c]]
  (map (fn [[dx dy]] [(+ r dx) (+ c dy)]) neighbordeltas
       ))

(defn build-board [state]
  (str/join "\n"  (for [r  (range rows)]
                    (str/join "" (for [c (range cols)] (char  (+ 48 (get state [r c])))))
                    ))
  )


(defn get-neighbors [state neighbors]
  (->>
   neighbors
   (filter #(get state % ))
   ))

(defn increase-all [board]
  (into {} (map (fn [[a b]] {a (inc b)}) board)))

(defn flash [ [row col] grid]
  (let [n (get-neighbor-coords [row col])
        n (get-neighbors grid  n)
        g (reduce (fn [b [r c]] (update b [r c] inc)) grid n )
        g (assoc g [row col] -9000)
        ]
    g)
  )


(defn reset-board [board]
  (into {} (map (fn [[a b]] (if (< b 0) {a 0} {a b}))board))
  )

(defn next-gen [[board numflashes]]
  (let [board (increase-all board)
        ]
    (loop [board board numflashes numflashes]
      (let [flashers (filter (fn [ [a b]] (> b 9)) board)
            nf (+ numflashes (count flashers))
            newboard (reduce (fn [b n] (flash n b)) board (map first flashers))
            ]
        (cond (= newboard board)
              
              [(reset-board newboard) nf]
              :else (recur  newboard nf))
        ))))

(def x (second  (loop [board board f 0 iter 0]
                  (if (= iter 100) [board f]
                      (let [r (next-gen [ board f])]
                        (recur (first r) (second r)  (inc iter))
                        )))
                ))


(defn part2-test [board]
  (let [cellcount  (count board)
        filtered (filter (fn [ [a b]] (= b 0))board)
        filtercount (count filtered)
        ]
    (= filtercount cellcount)
    ))


(loop [board board f 0 iter 0]
  (cond  (part2-test board) iter
         :else  (let [r (next-gen [ board f])]
                  (recur (first r) (second r)  (inc iter))
                  ))))

