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
   (slurp "data/sample09.dat")
   ;;(aocd/input 2021 9)
   (#(string/split % #"\n"))
   (map (fn [x] (map u/parse-int (string/split x #""))))
   (map  #(map-indexed (fn [i item] [i item]) %))
   (map-indexed (fn [j line]
                  (map (fn [[i k]] [j i k] ) line)
                  ) ) 
   first
   )

  )

(def grid (reduce (fn [sofar [x y item]]
                    (assoc sofar [x y] item))
                  {} data
                  ))

(defn part1 [grid]
  )

