(ns day01
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            ))
(def data
  (->> 
   ;;(slurp "data/sample01.dat")
   (aocd/input 2021 1)
   string/split-lines
   (map u/parse-int)
   ))

(defn increase-times [data]
  (dec (first 
        (reduce (fn [[inctimes prev] next]
                  (if (> next prev)
                    [(inc inctimes) next]
                    [inctimes next])
                  ) [0 0] data))))

(defn part1 [data] (increase-times data))
(defn part2 [data]
  (->> data
       (partition 3 1 data)
       (map #(reduce + %) )
       increase-times))

(def trip (map #(reduce + %)  (partition 3 1 data)))
(increase-times trip)
