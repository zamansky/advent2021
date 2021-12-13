(ns day13
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]))
(def data
  (->> 
   (slurp "data/sample13.dat")
   ;;(aocd/input 2021 13)
   ( #(string/split % #"\n\n"))
   ))

(def coords (->> (first data)
                 string/split-lines
                 (map #(string/split % #","))

                 (map (fn [x] (map #(u/parse-int %) x)))
                 
                 ))

(defn board->string [board]
  (let [maxx (apply max (map first (keys board)))
        minx (apply min (map first (keys board)))
        maxy (apply max (map second (keys board)))
        miny (apply min (map second (keys board)))
        k (keys board)
        grid (into [] (repeat (inc maxy) (into [] (repeat (inc maxx) \.))))
        filled-grid (reduce (fn [b [y x]] (assoc-in b [x y] \#)) grid k)
        string-grid (map #(apply str %) filled-grid)
        ]
    string-grid
    ))


(defn parse-fold-line [line]
  (let [tmp (string/split line #"=")
        amt (u/parse-int (second tmp))
        axis (last (first tmp))
        ]
    [axis amt]))


(defn new-coord [x y minx miny axis amt]
  [x y]
  (if (or
       (and (= axis \y)
            (> y amt))
       (and (= axis \x)
            (> x amt)))
    
    (cond (= axis \y)
          [x
           (- y (* 2 (u/abs (- y amt))))
           ]
          :else
          [(- x (* 2 (u/abs (- x amt))))
           y]
          )
    [x y]
    ))

(defn fold [board line]
  (let [maxx (apply max (map first (keys board)))
        minx (apply min (map first (keys board)))
        maxy (apply max (map second (keys board)))
        miny (apply min (map second (keys board)))
        k (keys board)
        [axis amt] (parse-fold-line line)
        ]
    (reduce (fn [b [x y]]
              (let [ [newx newy] (new-coord x y minx miny axis amt)]
                (assoc (dissoc b [x y]) [newx newy] \#)))
            board k)
    ))

(def board (coords->board coords))
(def folds (string/split-lines (second data)))

(defn part1 [board folds]
  (let [newboard (fold board (first folds))]
    (count (keys newboard))))

(defn part2 [board folds]
  (reduce (fn [b f] (fold board f)) board folds))
