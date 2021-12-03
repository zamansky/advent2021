(ns day03
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            [hashp.core]
            [clojure.math.numeric-tower :as math]
            ))


(def data
  (->> 
   ;;(slurp "data/sample03.dat")
   (aocd/input 2021 3)
   string/split-lines
   ;;(map u/parse-int)
   ;;(map (fn [x] (->>  (string/split x #" ")
   (map (fn [x] (map u/parse-int (string/split x #""))))
   ))


(defn calc-ones [data]
  (reduce (fn [sofar next]
            (map + sofar next)
            )
          (take (count (first data)) (repeat 0))data))


(defn calc-gamma [data]
  (let [ones (calc-ones data)
        l (count data)]
    (map (fn [x] (if (>= x  (/ l 2)) 1 0)) ones)))

(defn calc-epsilon [data]
  (let [ones (calc-ones data)
        l (count data)]
    (map (fn [x] (if (>= x  (/ l 2)) 0 1)) ones)))


(def gamma (calc-gamma data))
(def epsilon (calc-epsilon data))

(defn binlist->dec [str]
(reduce (fn [sofar next]
          (+ (* 2 sofar) next))
        0 str))
(def part1-ans (* (binlist->dec gamma) (binlist->dec epsilon)))

(defn calc-ox-co2 [ data calc-func]
  (loop [pos 0
         newdata data
         ]
    (let [key (calc-func newdata)
          
          ]
      (if (or  (> pos (count (first newdata)))
               (=  (count newdata) 1)) (first newdata)
          (recur (inc  pos)    (filter (fn [x] (=  (nth x pos)  (nth key pos))) newdata))))))

(def part2-ans (* (binlist->dec (calc-ox-co2 data calc-gamma))
                  (binlist->dec (calc-ox-co2 data calc-epsilon))))
