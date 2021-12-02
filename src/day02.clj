(ns day02
  (:require [clojure.string :as string]
            [utils :as u]
            [aocd.core :as aocd]
            ))

(def data
  (->> 
   ;;(slurp "data/sample02.dat")
   (aocd/input 2021 2)
   string/split-lines
   ;;(map u/parse-int)
   (map (fn [x] (->>  (string/split x #" ")
                      (#(let [[a  b] %] [a (u/parse-int b)]))
                      )))
   ))

(defn solve-part1 [data]
  (apply *(reduce (fn [[h d] [inst amt] ]
                    (cond
                      (= inst "forward") [h (+ d amt)]
                      (= inst "down") [ (+ h amt) d]
                      (= inst "up") [ (max 0 (- h amt)) d]
                      :else [h d])
                    ) [0 0] data)))

(defn solve-part2 [data]
  (apply * (drop 1 (reduce (fn [[aim h d] [inst amt] ]
                             (cond
                               (= inst "forward") [aim (+ h amt) (+ d (* aim amt))] 
                               (= inst "down") [(+ aim amt)  h d]
                               (= inst "up") [(- aim amt) h d]
                               :else [aim h d])
                             ) [0 0 0] data))))
