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

(def a [0 1 1 2 1 0 0 0 0])

(def start
  (reduce (fn [sofar next]
            (update sofar next inc)
            ) [0 0 0 0 0 0 0 0 0] data )
  )
(def ans (apply + (loop [i 0 gens start ]
                    (if (= i 256)
                      gens
                      (let [last  (first  gens)
                            gens  (assoc gens 7 (+ (nth gens 7) (first gens))) ;; 7 will become 6 after the drop 
                            gens (conj gens last)
                            gens (into [] (drop 1 gens))
                            ]
                        (recur (inc i)
                               gens))))))
