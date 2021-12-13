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

(defn build-board [state]
  (str/join "\n"  (for [r  (range rows)]
                    (str/join "" (for [c (range cols)] (char  (+ 48 (get state [r c])))))
                    ))
  )

