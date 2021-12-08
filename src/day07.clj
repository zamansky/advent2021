(ns day07
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))


(def data
  (->> 
   ;;(slurp "data/sample07.dat")
   ;;"16,1,2,0,4,2,7,1,2,14"
   (aocd/input 2021 7)
   (#(string/split % #","))
   (map u/parse-int)))


(defn part1-func [location crab]
  (u/abs (- location crab)))

(defn part2-func [location crab]
  (let [diff (u/abs (- location crab))
        ans (apply + (range 1 (+ 1 diff)))
        ]
    ans)
  )

(defn part2-func-fast [location crab]
  (let [diff (u/abs (- location crab))
        ans (int (* diff (inc diff) 0.5))
        ]
    ans)
  )

(defn calc-cost [func data]
  (let [start (apply min data) stop (apply max data)]
    (apply min (loop [result []
                      i start]
                 (let [this-step  (map #(func i %) data)
                       ans (apply + this-step)
                       newans (conj result ans)]
                   (if (= i stop)
                     newans
                     (recur newans (inc i)))

                   )))))

)


