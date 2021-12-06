(ns day06
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))


(def data
  (->> 
   ;; (slurp "data/sample05.dat")
   ;;"3,4,3,1,2"
   (aocd/input 2021 6) 
   (#(string/split % #","))
   (map u/parse-int)
   )
  )



(defn solve [data days]
  (let [start-state (reduce (fn [sofar next] (update sofar next inc)) [0 0 0 0 0 0 0 0 0] data )]
    (apply + (loop [i 0
                    gens start-state]
               (if (< i days)
                 (let [last (first gens)
                       gens  (into []  ( drop 1 gens))
                       gens  (assoc gens 6 (+ (nth gens 6) last))
                       gens  (conj gens last)
                       ]
                   (recur (inc i) gens)
                   )
                 gens
                 )))))

